{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           ListT
import           Network.Ethereum.Web3
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import qualified Network.Ethereum.Web3.Address as Address
import           Servant
import qualified STMContainers.Map as Map

import           EthInterface

type ServerState = Map.Map Text (CreditRecord Signed)

type API = "transactions" :> Get '[JSON] [IssueCreditLog]
      :<|> "pending" :> Get '[JSON] [(Text, CreditRecord Signed)]
      :<|> "submit" :> ReqBody '[JSON] (CreditRecord Unsigned) :> Post '[JSON] SubmissionResponse

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
submitHandler record@(CreditRecord creditor _ _ user) = do
    creditMap <- ask
    -- TODO handle this appropriately
    Right (nonce, hash, signedRecord) <- liftIO $ signCreditRecord record

    -- check if hash is already registered in pending txs
    val <- liftIO . atomically $ Map.lookup hash creditMap

    case val of
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            -- if the submitted credit record has a matching pending record,
            -- finalize the transaction on the blockchain
            if creditor /= user
                then finalizeTransaction (signature storedRecord)
                                         (signature signedRecord)
                                         signedRecord
                else finalizeTransaction (signature signedRecord)
                                         (signature storedRecord)
                                         signedRecord
            -- delete pending record after transaction finalization
            atomically $ Map.delete hash creditMap

        -- if no matching transaction is found, create pending transaction
        Nothing -> liftIO . atomically $ Map.insert signedRecord hash creditMap

    return $ SubmissionResponse hash nonce

api :: Proxy API
api = Proxy

server :: ServerT API (ReaderT ServerState IO)
server = transactionsHandler
    :<|> pendingHandler
    :<|> submitHandler

readerToHandler' :: forall a. ServerState -> ReaderT ServerState IO a -> Handler a
readerToHandler' state r = liftIO (runReaderT r state)

readerToHandler :: ServerState -> ReaderT ServerState IO :~> Handler
readerToHandler state = NT (readerToHandler' state)

readerServer :: ServerState -> Server API
readerServer state = enter (readerToHandler state) server

app :: ServerState -> Application
app state = serve api (readerServer state)

main :: IO ()
main = do
    pendingMap <- atomically Map.new
    N.run 80 $ app pendingMap
