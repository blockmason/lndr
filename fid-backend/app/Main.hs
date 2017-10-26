{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

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
--       :<|> "pending" :> Get '[JSON] [SignedCredit]
      :<|> "submit"   :> ReqBody '[JSON] SignedCredit :> Post '[JSON] ServerResponse

server :: Server API
server = transactionsHandler
    :<|> submitHandler

transactionsHandler :: Handler [IssueCreditLog]
transactionsHandler = do
    a <- runWeb3 fidLogs
    return $ case a of
                Right ls -> ls
                Left _ -> []

submitHandler :: SignedCredit -> Handler ServerResponse
submitHandler signedCredit = return $ ServerResponse 10

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
