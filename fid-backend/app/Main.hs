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
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Lens
import           Data.Aeson
import           GHC.Generics
import           Network.Ethereum.Web3
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import qualified Network.Ethereum.Web3.Address as Address
import           Servant
import           STMContainers.Map

import Web3Interface

data UcacCreationLog = UcacCreationLog { ucac :: String }
                        deriving Generic

instance ToJSON UcacCreationLog

type API = "transactions" :> Get '[JSON] [IssueCreditLog]
      :<|> "pending" :> Get '[JSON] [CreditRecord Signed]
      :<|> "submit" :> ReqBody '[JSON] (CreditRecord Signed) :> Post '[JSON] ServerResponse

server :: Server API
server = transactionsHandler
    :<|> pendingHandler
    :<|> submitHandler

transactionsHandler :: Handler [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []

pendingHandler :: Handler [CreditRecord Signed]
pendingHandler = return []

submitHandler :: CreditRecord Signed -> Handler ServerResponse
submitHandler (CreditRecord _ _ a _) = return $ ServerResponse $ fromInteger a

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
