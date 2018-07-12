{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.PushData where

import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Lndr.Db.Types
import           Lndr.Types
import           Network.Ethereum.Web3

insertPushDatum :: Address -> Text -> Text -> Connection -> IO Int
insertPushDatum addr channelID platform conn = fromIntegral <$>
    execute conn "INSERT INTO push_data (address, channel_id, platform) VALUES (?,?,?) ON CONFLICT (address) DO UPDATE SET (channel_id, platform) = (EXCLUDED.channel_id, EXCLUDED.platform)" (addr, channelID, platform)


lookupPushDatumByAddress :: Address -> Connection -> IO (Maybe (Text, DevicePlatform))
lookupPushDatumByAddress addr conn = listToMaybe <$> query conn "SELECT channel_id, platform FROM push_data WHERE address = ?" (Only addr)


deletePushDatum :: Address -> Text -> Text -> Connection -> IO Int
deletePushDatum addr channelID platform conn = fromIntegral <$>
    execute conn "DELETE FROM push_data WHERE address = ? AND channel_id = ? AND platform = ?" (addr, channelID, platform)
