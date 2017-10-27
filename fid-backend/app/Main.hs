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
import           GHC.Generics
import           ListT
import           Network.Ethereum.Web3
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import qualified Network.Ethereum.Web3.Address as Address
import           Servant
import qualified STMContainers.Map as Map

import Web3Interface

data UcacCreationLog = UcacCreationLog { ucac :: String }
                        deriving Generic

instance ToJSON UcacCreationLog

type API = "transactions" :> Get '[JSON] [IssueCreditLog]
      :<|> "pending" :> Get '[JSON] [CreditRecord Signed]
      :<|> "submit" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] ServerResponse

transactionsHandler :: ReaderT ServerState IO [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []

pendingHandler :: ReaderT ServerState IO [CreditRecord Signed]
pendingHandler = do creditMap <- ask
                    list <- liftIO . atomically . toList $ Map.stream creditMap
                    return $ snd <$> list


submitHandler :: CreditRecord Signed -> ReaderT ServerState IO ServerResponse
submitHandler r@(CreditRecord _ _ a _) = do liftIO $ print r
                                            creditMap <- ask
                                            liftIO . atomically $ Map.insert r "hi" creditMap
                                            list <- liftIO . atomically . toList $ Map.stream creditMap
                                            liftIO $ print list
                                            return $ ServerResponse $ fromInteger a

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



-- readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
-- readerToExcept cfg =
--   Nat $ \appm -> evalStateT (runReaderT appm cfg) Map.empty
--
readerServer :: ServerState -> Server API
readerServer state = enter (readerToHandler state) server

-- testServer' :: Int -> Server TestAPI
-- testServer' code = enter (Nat $ liftIO . (`runReaderT` code)) testServerT

type ServerState = Map.Map Text (CreditRecord Signed)

type App = ReaderT ServerState (ExceptT ServantErr IO)

app :: ServerState -> Application
app state = serve api (readerServer state)

main :: IO ()
main = do
    pendingMap <- atomically Map.new
    N.run 80 $ app pendingMap
