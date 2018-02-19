{-# LANGUAGE OverloadedStrings         #-}

module Lndr.Config where

import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict     as H (lookup)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text               as T
import           Lndr.Types
import           System.FilePath

loadConfig :: IO ServerConfig
loadConfig = do
    config <- getMap =<< load [Required $ "lndr-backend" </> "data" </> "lndr-server.config"]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
    return $ ServerConfig (loadEntry "lndrUcacAddr")
                          (loadEntry "creditProtocolAddress")
                          (loadEntry "issueCreditEvent")
                          (loadEntry "scanStartBlock")
                          (loadEntry "dbUser")
                          (loadEntry "dbUserPassword")
                          (loadEntry "dbName")
                          (loadEntry "executionAddress")
                          (loadEntry "gasPrice")
                          (loadEntry "maxGas")
                          (loadEntry "urbanAirshipKey")
                          (loadEntry "urbanAirshipSecret")
                          (loadEntry "heartbeatInterval")
                          (loadEntry "awsPhotoBucket")
                          (loadEntry "accessKeyId")
                          (loadEntry "secretAccessKey")
                          (loadEntry "postgresHost")
                          (loadEntry "postgresPort")
                          (loadEntry "web3Host")
                          (loadEntry "web3Port")
