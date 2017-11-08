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

type ServerState = Map.Map Text PendingRecord

freshState :: forall k v. IO (Map.Map k v)
freshState = atomically Map.new

type LndrAPI =
        "transactions" :> Get '[JSON] (LndrResponse [IssueCreditLog])
   :<|> "pending" :> Get '[JSON] (LndrResponse [PendingRecord])
   :<|> "lend" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] (LndrResponse ())
   :<|> "borrow" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] (LndrResponse ())
   :<|> "reject" :> ReqBody '[JSON] RejectRecord :> Post '[JSON] (LndrResponse ())
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] (LndrResponse Nonce)
   -- TODO change this to accept JSON object w/ signature
   :<|> "nick" :> Capture "address" Address :> Capture "nick" Text :> Post '[JSON] (LndrResponse ())
   :<|> "docs" :> Raw

lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy


nickHandler :: Address -> Text -> LndrHandler (LndrResponse ())
nickHandler _ _ = undefined


-- submit a signed message consisting of "REJECT + CreditRecord HASH"
-- each credit record will be referenced by its hash
rejectHandler :: RejectRecord -> LndrHandler (LndrResponse ())
rejectHandler = undefined


transactionsHandler :: LndrHandler (LndrResponse [IssueCreditLog])
transactionsHandler = do
    a <- runWeb3 lndrLogs
    return . LndrResponse 200 . Just $ case a of
                Right ls -> ls
                Left _ -> []


pendingHandler :: LndrHandler (LndrResponse [PendingRecord])
pendingHandler = do
    creditMap <- ask
    fmap (LndrResponse 200 . Just . fmap snd) . liftIO . atomically . toList $ Map.stream creditMap


lendHandler :: CreditRecord Signed -> LndrHandler (LndrResponse ())
lendHandler cr@(CreditRecord creditor _ _ _ _) = submitSignedHandler creditor cr


borrowHandler :: CreditRecord Signed -> LndrHandler (LndrResponse ())
borrowHandler cr@(CreditRecord _ debtor _ _ _) = submitSignedHandler debtor cr


submitSignedHandler :: Text -> CreditRecord Signed
                    -> LndrHandler (LndrResponse ())
submitSignedHandler submitterAddress signedRecord@(CreditRecord creditor _ _ _ sig) = do
    creditMap <- ask
    Right (nonce, hash) <- liftIO . runWeb3 $ hashCreditRecord signedRecord

    -- TODO TODO verify sig

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


    return $ LndrResponse 200 Nothing
    where submitterAddr = textToAddress submitterAddress


nonceHandler :: Address -> Address -> LndrHandler (LndrResponse Nonce)
nonceHandler p1 p2 = do
    a <- runWeb3 $ queryNonce p1 p2
    return . LndrResponse 200 . Just . Nonce $ case a of
                Right ls -> ls
                Left _ -> 0 -- TODO fix this, get proper exception handling in place
