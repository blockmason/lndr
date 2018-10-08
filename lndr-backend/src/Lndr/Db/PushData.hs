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
    execute conn "INSERT INTO push_data (channel_id, address, platform) VALUES (?,?,?) ON CONFLICT (channel_id) DO UPDATE SET (address, platform) = (EXCLUDED.address, EXCLUDED.platform)" (channelID, addr, platform)


lookupPushDatumByAddress :: Address -> Connection -> IO (Maybe (Text, DevicePlatform))
lookupPushDatumByAddress addr conn = listToMaybe <$> query conn "SELECT channel_id, platform FROM push_data WHERE address = ?" (Only addr)


deletePushDatum :: Address -> Text -> Text -> Connection -> IO Int
deletePushDatum addr channelID platform conn = fromIntegral <$>
    execute conn "DELETE FROM push_data WHERE address = ? AND channel_id = ? AND platform = ?" (addr, channelID, platform)
