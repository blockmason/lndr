{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Web3Interface

type ServerState = Map.Map Text (CreditRecord Signed)

type API = "transactions" :> Get '[JSON] [IssueCreditLog]
      :<|> "pending" :> Get '[JSON] [(Text, CreditRecord Signed)]
      :<|> "submit" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] ServerResponse

transactionsHandler :: ReaderT ServerState IO [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []

pendingHandler :: ReaderT ServerState IO [(Text, CreditRecord Signed)]
pendingHandler = do creditMap <- ask
                    list <- liftIO . atomically . toList $ Map.stream creditMap
                    return $ list

submitHandler :: CreditRecord Signed -> ReaderT ServerState IO ServerResponse
submitHandler record = do
    creditMap <- ask
    Right (nonce, hash) <- liftIO $ signatureAndNonceFromCreditRecord record
    liftIO . atomically $ Map.insert record hash creditMap
    return $ ServerResponse hash nonce

api :: Proxy API
api = Proxy

readerToHandler' :: forall a. ServerState -> ReaderT ServerState IO a -> Handler a
readerToHandler' state r = liftIO (runReaderT r state)

readerToHandler :: ServerState -> ReaderT ServerState IO :~> Handler
readerToHandler state = NT (readerToHandler' state)


server :: ServerT API (ReaderT ServerState IO)
server = transactionsHandler
    :<|> pendingHandler
    :<|> submitHandler


readerServer :: ServerState -> Server API
readerServer state = enter (readerToHandler state) server

app :: ServerState -> Application
app state = serve api (readerServer state)

main :: IO ()
main = do
    pendingMap <- atomically Map.new
    N.run 80 $ app pendingMap
