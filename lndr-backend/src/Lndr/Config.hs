{-# LANGUAGE OverloadedStrings         #-}

module Lndr.Config where

import qualified Data.Bimap              as B
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Default
import           Data.Either.Combinators (fromRight)
import qualified Data.HashMap.Strict     as H (lookup)
import           Data.Map                as M
import           Data.Maybe              (fromJust, fromMaybe)
import qualified Data.Text               as T
import           Lndr.Types
import qualified Network.Ethereum.Web3.Address as Addr
import           Network.Ethereum.Util
import           System.Environment      (setEnv)
import           System.FilePath

loadConfig :: IO ServerConfig
loadConfig = do
    config <- getMap =<< load [Required $ "lndr-backend" </> "data" </> "lndr-server.config"]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
        privateKey = loadEntry "execution-private-key"
        execAddress = fromRight (error "bad privkey") . Addr.fromText
                                                      . fromJust $ privateToAddress privateKey
    return $ ServerConfig (B.fromList [ ("USD", loadEntry "lndr-ucacs.usd")
                                      , ("JPY", loadEntry "lndr-ucacs.jpy")
                                      , ("KRW", loadEntry "lndr-ucacs.krw")
                                      , ("DKK", loadEntry "lndr-ucacs.dkk")
                                      , ("CHF", loadEntry "lndr-ucacs.chf")
                                      , ("CNY", loadEntry "lndr-ucacs.cny")
                                      , ("EUR", loadEntry "lndr-ucacs.eur")
                                      , ("AUD", loadEntry "lndr-ucacs.aud")
                                      , ("GBP", loadEntry "lndr-ucacs.gbp")
                                      , ("CAD", loadEntry "lndr-ucacs.cad")
                                      , ("NOK", loadEntry "lndr-ucacs.nok")
                                      , ("SEK", loadEntry "lndr-ucacs.sek")
                                      , ("NZD", loadEntry "lndr-ucacs.nzd") ])
                          (loadEntry "bind-address")
                          (loadEntry "bind-port")
                          (loadEntry "credit-protocol-address")
                          (loadEntry "issue-credit-event")
                          (loadEntry "scan-start-block")
                          (loadEntry "db.user")
                          (loadEntry "db.user-password")
                          (loadEntry "db.name")
                          (loadEntry "db.host")
                          (loadEntry "db.port")
                          (loadEntry "gas-price")
                          def
                          (loadEntry "max-gas")
                          0
                          (loadEntry "urban-airship.key")
                          (loadEntry "urban-airship.secret")
                          (loadEntry "heartbeat-interval")
                          (loadEntry "aws.photo-bucket")
                          (loadEntry "aws.access-key-id")
                          (loadEntry "aws.secret-access-key")
                          (loadEntry "web3-url")
                          privateKey
                          execAddress
                          0
