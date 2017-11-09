{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy (ByteString)
import           Data.Either.Combinators (mapLeft)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy (pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           EthInterface
import           ListT
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant
import           Servant.API
import           Servant.Docs
import qualified STMContainers.Map as Map

newtype LndrHandler a = LndrHandler {
  runLndr :: ReaderT ServerState (ExceptT LndrError IO) a
} deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError LndrError, MonadIO)
instance ToHttpApiData Addr.Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Addr.Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

data ServerState = ServerState { pendingMap :: Map.Map Text PendingRecord
                               , nickMap :: Map.Map Address Text
                               , friendlistMap :: Map.Map Address [Address]
                               }

freshState :: IO ServerState
freshState = ServerState <$> atomically Map.new
                         <*> atomically Map.new
                         <*> atomically Map.new

type LndrAPI =
        "transactions" :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> Get '[JSON] [PendingRecord]
   :<|> "lend" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "borrow" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "reject" :> ReqBody '[JSON] RejectRecord :> Post '[JSON] NoContent
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Nonce
   :<|> "nick" :> ReqBody '[JSON] NickRequest :> PostNoContent '[JSON] NoContent
   :<|> "friends" :> Capture "user" Address :> Get '[JSON] [Address]
   :<|> "docs" :> Raw

lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy


web3ToLndr :: Show a => IO (Either a b) -> LndrHandler b
web3ToLndr = LndrHandler . lift . ExceptT . fmap (mapLeft (LndrError . show))


lndrWeb3 :: Web3 DefaultProvider b -> LndrHandler b
lndrWeb3 = web3ToLndr . runWeb3


nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler _ = undefined

friendHandler :: Address -> LndrHandler [Address]
friendHandler _ = undefined


-- submit a signed message consisting of "REJECT + CreditRecord HASH"
-- each credit record will be referenced by its hash
rejectHandler :: RejectRecord -> LndrHandler NoContent
rejectHandler = undefined


transactionsHandler :: LndrHandler [IssueCreditLog]
transactionsHandler = lndrWeb3 lndrLogs


pendingHandler :: LndrHandler [PendingRecord]
pendingHandler = do
    creditMap <- pendingMap <$> ask
    fmap (fmap snd) . liftIO . atomically . toList $ Map.stream creditMap


lendHandler :: CreditRecord Signed -> LndrHandler NoContent
lendHandler cr@(CreditRecord creditor _ _ _ _) = submitSignedHandler creditor cr


borrowHandler :: CreditRecord Signed -> LndrHandler NoContent
borrowHandler cr@(CreditRecord _ debtor _ _ _) = submitSignedHandler debtor cr


submitSignedHandler :: Text -> CreditRecord Signed
                    -> LndrHandler NoContent
submitSignedHandler submitterAddress signedRecord@(CreditRecord creditor _ _ _ sig) = do
    creditMap <- pendingMap <$> ask
    (nonce, hash) <- lndrWeb3 $ hashCreditRecord signedRecord

    -- TODO TODO verify sig
    --
    -- submitter is one of creditor or debtor
    signer <- LndrHandler . return $ EU.ecrecover sig $ EU.hashPersonalMessage hash

    -- submitter signed the tx
    -- submitter == creditor || submitter == debtor

    -- check if hash is already registered in pending txs
    pendingRecord <- liftIO . atomically $ Map.lookup hash creditMap
    let cr = creditRecord <$> pendingRecord

    case cr of
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            -- if the submitted credit record has a matching pending record,
            -- finalize the transaction on the blockchain
            if creditor /= submitterAddress
                then finalizeTransaction (signature storedRecord)
                                         (signature signedRecord)
                                         storedRecord
                else finalizeTransaction (signature signedRecord)
                                         (signature storedRecord)
                                         storedRecord
            -- delete pending record after transaction finalization
            atomically $ Map.delete hash creditMap

        -- if no matching transaction is found, create pending transaction
        Nothing -> liftIO . atomically $
                        Map.insert (PendingRecord signedRecord submitterAddr hash)
                                   hash creditMap

    return NoContent
    where submitterAddr = textToAddress submitterAddress


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = fmap Nonce . web3ToLndr . runWeb3 $ queryNonce p1 p2
