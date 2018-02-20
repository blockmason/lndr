{-# LANGUAGE OverloadedStrings         #-}

module Lndr.Config where

import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict     as H (lookup)
import           Data.Map                as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text               as T
import           Lndr.Types
import           System.Environment      (setEnv)
import           System.FilePath

loadConfig :: IO ServerConfig
loadConfig = do
    config <- getMap =<< load [Required $ "lndr-backend" </> "data" </> "lndr-server.config"]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
    return $ ServerConfig (M.fromList [ ("USD", loadEntry "lndr-ucacs.usd")
                                      , ("JPY", loadEntry "lndr-ucacs.jpy")
                                      , ("KRW", loadEntry "lndr-ucacs.krw") ])
                          (loadEntry "creditProtocolAddress")
                          (loadEntry "issueCreditEvent")
                          (loadEntry "scanStartBlock")
                          (loadEntry "db.user")
                          (loadEntry "db.userPassword")
                          (loadEntry "db.name")
                          (loadEntry "db.host")
                          (loadEntry "db.port")
                          (loadEntry "executionAddress")
                          (loadEntry "gasPrice")
                          (loadEntry "maxGas")
                          (loadEntry "urban-airship.key")
                          (loadEntry "urban-airship.secret")
                          (loadEntry "heartbeatInterval")
                          (loadEntry "aws.photoBucket")
                          (loadEntry "aws.accessKeyId")
                          (loadEntry "aws.secretAccessKey")
                          (loadEntry "web3Url")


web3ProviderEnvVariable :: String
web3ProviderEnvVariable = "WEB3_PROVIDER"


setEnvironmentConfigs :: ServerConfig -> IO ()
setEnvironmentConfigs config = setEnv web3ProviderEnvVariable (web3Url config)
