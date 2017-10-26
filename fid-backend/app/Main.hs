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
import           Servant
import           STMContainers.Map

data UcacCreationLog = UcacCreationLog { ucac :: String }
                        deriving Generic

instance ToJSON UcacCreationLog

data TransactionLog = TransactionLog { ucac :: String }
    deriving Generic

instance ToJSON TransactionLog

type LogsAPI = "ucacs" :> Get '[JSON] [UcacCreationLog]
          :<|> "fid"   :> Get '[JSON] [TransactionLog]

logsServer :: Server LogsAPI
logsServer = return [UcacCreationLog "hi"]
        :<|> return [TransactionLog "goodbye"]

api :: Proxy LogsAPI
api = Proxy

app :: Application
app = serve api logsServer

main :: IO ()
main = N.run 80 app
