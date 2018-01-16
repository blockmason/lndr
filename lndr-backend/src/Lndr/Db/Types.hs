{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Types where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3

-- DB Typeclass instances

instance ToField Address where
    toField = Escape . addrToBS

instance FromField Address where
    fromField f dat = textToAddress <$> fromField f dat

instance FromField DevicePlatform where
    fromField f dat = toDevicePlatform <$> fromField f dat
        where toDevicePlatform :: Text -> DevicePlatform
              toDevicePlatform "ios" = Ios
              toDevicePlatform "android" = Android

instance FromRow CreditRecord

instance FromRow IssueCreditLog

instance FromRow NickInfo
