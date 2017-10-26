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
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import qualified Network.Ethereum.Web3.Address as Address
import           Servant
import           STMContainers.Map

import Web3Interface

data UcacCreationLog = UcacCreationLog { ucac :: String }
                        deriving Generic

instance ToJSON UcacCreationLog

data PendingTransaction = PendingTransaction { }

type API = "ucacs" :> Get '[JSON] [UcacCreationLog]
      :<|> "fid"   :> Get '[JSON] [IssueCreditLog]

server :: Server API
server = return [UcacCreationLog "hi"]
    :<|> return [IssueCreditLog Address.zero Address.zero Address.zero 0]

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
