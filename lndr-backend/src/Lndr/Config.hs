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

loadConfig :: FilePath -> IO ServerConfig
loadConfig configPath = do
    config <- getMap =<< load [Required configPath]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
        privateKey = loadEntry "execution-private-key"
        execAddress = fromRight (error "bad privkey") . Addr.fromText
                                                      . fromJust $ privateToAddress privateKey
    return $ ServerConfig (B.fromList [ ("AUD", loadEntry "lndr-ucacs.aud")
                                      , ("CAD", loadEntry "lndr-ucacs.cad")
                                      , ("CHF", loadEntry "lndr-ucacs.chf")
                                      , ("CNY", loadEntry "lndr-ucacs.cny")
                                      , ("DKK", loadEntry "lndr-ucacs.dkk")
                                      , ("EUR", loadEntry "lndr-ucacs.eur")
                                      , ("GBP", loadEntry "lndr-ucacs.gbp")
                                      , ("HKD", loadEntry "lndr-ucacs.hkd")
                                      , ("IDR", loadEntry "lndr-ucacs.idr")
                                      , ("ILS", loadEntry "lndr-ucacs.ils")
                                      , ("INR", loadEntry "lndr-ucacs.inr")
                                      , ("JPY", loadEntry "lndr-ucacs.jpy")
                                      , ("KRW", loadEntry "lndr-ucacs.krw")
                                      , ("MYR", loadEntry "lndr-ucacs.myr")
                                      , ("NOK", loadEntry "lndr-ucacs.nok")
                                      , ("NZD", loadEntry "lndr-ucacs.nzd")
                                      , ("PLN", loadEntry "lndr-ucacs.pln")
                                      , ("RUB", loadEntry "lndr-ucacs.rub")
                                      , ("SEK", loadEntry "lndr-ucacs.sek")
                                      , ("SGD", loadEntry "lndr-ucacs.sgd")
                                      , ("THB", loadEntry "lndr-ucacs.thb")
                                      , ("TRY", loadEntry "lndr-ucacs.try")
                                      , ("USD", loadEntry "lndr-ucacs.usd")
                                      , ("VND", loadEntry "lndr-ucacs.vnd") ])
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
                          (loadEntry "heartbeat-interval")
                          (loadEntry "aws.photo-bucket")
                          (loadEntry "aws.access-key-id")
                          (loadEntry "aws.secret-access-key")
                          (loadEntry "notifications.api-url")
                          (loadEntry "notifications.api-key")
                          (loadEntry "web3-url")
                          privateKey
                          execAddress
                          0
