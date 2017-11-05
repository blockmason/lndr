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

type ServerState = Map.Map Text (CreditRecord Signed)

freshState :: forall k v. IO (Map.Map k v)
freshState = atomically Map.new

type FiddyAPI =
        "transactions" :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> Get '[JSON] [(Text, CreditRecord Signed)]
   :<|> "submit" :> ReqBody '[JSON] (CreditRecord Unsigned) :> Post '[JSON] SubmissionResponse
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Integer
   :<|> "docs" :> Raw

fiddyAPI :: Proxy FiddyAPI
fiddyAPI = Proxy

transactionsHandler :: ReaderT ServerState IO [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []


pendingHandler :: ReaderT ServerState IO [(Text, CreditRecord Signed)]
pendingHandler = do creditMap <- ask
                    liftIO . atomically . toList $ Map.stream creditMap


submitHandler :: CreditRecord Unsigned -> ReaderT ServerState IO SubmissionResponse
submitHandler record@(CreditRecord creditor _ _ _ user) = do
    creditMap <- ask
    -- TODO handle this appropriately
    Right (nonce, hash, signedRecord) <- liftIO . runExceptT $ signCreditRecord record

    -- check if hash is already registered in pending txs
    val <- liftIO . atomically $ Map.lookup hash creditMap

    case val of
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
        Nothing -> liftIO . atomically $ Map.insert signedRecord hash creditMap

    return $ SubmissionResponse hash nonce

nonceHandler :: Address -> Address -> ReaderT ServerState IO Integer
nonceHandler p1 p2 = do
    a <- runWeb3 $ queryNonce p1 p2
    return $ case a of
                Right ls -> ls
                Left _ -> 0 -- TODO fix this, get proper exception handling in place
