{-# LANGUAGE OverloadedStrings         #-}

module Lndr.Config where

import           Data.Configurator
import           Data.Configurator.Types
import           Data.Default
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
                          (loadEntry "credit-protocol-address")
                          (loadEntry "issue-credit-event")
                          (loadEntry "scan-start-block")
                          (loadEntry "db.user")
                          (loadEntry "db.user-password")
                          (loadEntry "db.name")
                          (loadEntry "db.host")
                          (loadEntry "db.port")
                          (loadEntry "execution-address")
                          (loadEntry "gas-price")
                          def
                          (loadEntry "max-gas")
                          (loadEntry "urban-airship.key")
                          (loadEntry "urban-airship.secret")
                          (loadEntry "heartbeat-interval")
                          (loadEntry "aws.photo-bucket")
                          (loadEntry "aws.access-key-id")
                          (loadEntry "aws.secret-access-key")
                          (loadEntry "web3-url")


web3ProviderEnvVariable :: String
web3ProviderEnvVariable = "WEB3_PROVIDER"


setEnvironmentConfigs :: ServerConfig -> IO ()
setEnvironmentConfigs config = setEnv web3ProviderEnvVariable (web3Url config)
