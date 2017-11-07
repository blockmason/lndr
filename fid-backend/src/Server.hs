{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           ListT
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant
import           Servant.API
import           Servant.Docs
import qualified STMContainers.Map as Map

import           EthInterface

instance ToHttpApiData Addr.Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Addr.Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

type ServerState = Map.Map Text PendingRecord

freshState :: forall k v. IO (Map.Map k v)
freshState = atomically Map.new

type LndrAPI =
        "transactions" :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> Get '[JSON] [PendingRecord]
   :<|> "submit" :> ReqBody '[JSON] (CreditRecord Unsigned) :> Post '[JSON] SubmissionResponse
   :<|> "lend" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] SubmissionResponse
   :<|> "borrow" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] SubmissionResponse
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Integer
   :<|> "docs" :> Raw

lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy

transactionsHandler :: ReaderT ServerState IO [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []


pendingHandler :: ReaderT ServerState IO [PendingRecord]
pendingHandler = do
    creditMap <- ask
    fmap (fmap snd) . liftIO . atomically . toList $ Map.stream creditMap


submitHandler :: CreditRecord Unsigned -> ReaderT ServerState IO SubmissionResponse
submitHandler record@(CreditRecord creditor _ _ _ user) = do
    creditMap <- ask
    -- TODO handle this appropriately
    Right (nonce, hash, signedRecord) <- liftIO . runExceptT $ signCreditRecord record

    -- check if hash is already registered in pending txs
    pendingRecord <- liftIO . atomically $ Map.lookup hash creditMap
    let cr = creditRecord <$> pendingRecord

    case cr of
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            -- if the submitted credit record has a matching pending record,
            -- finalize the transaction on the blockchain
            if creditor /= user
                then finalizeTransaction (signature storedRecord)
                                         (signature signedRecord)
                                         storedRecord
                else finalizeTransaction (signature signedRecord)
                                         (signature storedRecord)
                                         storedRecord
            -- delete pending record after transaction finalization
            atomically $ Map.delete hash creditMap

        -- if no matching transaction is found, create pending transaction
        Nothing -> liftIO . atomically $ Map.insert (PendingRecord signedRecord userAddr)
                                                    hash creditMap

    return $ SubmissionResponse hash nonce
    where userAddr = textToAddress user

lendHandler :: CreditRecord Signed -> ReaderT ServerState IO SubmissionResponse
lendHandler cr@(CreditRecord creditor _ _ _ _) = submitSignedHandler creditor cr


borrowHandler :: CreditRecord Signed -> ReaderT ServerState IO SubmissionResponse
borrowHandler cr@(CreditRecord _ debtor _ _ _) = submitSignedHandler debtor cr


submitSignedHandler :: Text -> CreditRecord Signed
                    -> ReaderT ServerState IO SubmissionResponse
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
        Nothing -> liftIO . atomically $ Map.insert (PendingRecord signedRecord submitterAddr)
                                                    hash creditMap


    return $ SubmissionResponse hash nonce
    where submitterAddr = textToAddress submitterAddress

nonceHandler :: Address -> Address -> ReaderT ServerState IO Integer
nonceHandler p1 p2 = do
    a <- runWeb3 $ queryNonce p1 p2
    return $ case a of
                Right ls -> ls
                Left _ -> 0 -- TODO fix this, get proper exception handling in place
