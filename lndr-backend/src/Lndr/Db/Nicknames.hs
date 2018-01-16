{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Nicknames where

import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Lndr.Db.Types
import           Lndr.Types
import           Network.Ethereum.Web3


insertNick :: Address -> Text -> Connection -> IO Int
insertNick addr nick conn = fromIntegral <$>
    execute conn "INSERT INTO nicknames (address, nickname) VALUES (?,?) ON CONFLICT (address) DO UPDATE SET nickname = EXCLUDED.nickname" (addr, nick)


lookupNick :: Address -> Connection -> IO (Maybe Text)
lookupNick addr conn = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT nickname FROM nicknames WHERE address = ?" (Only addr) :: IO [Only Text])

lookupAddressByNick :: Text -> Connection -> IO (Maybe NickInfo)
lookupAddressByNick nick conn = listToMaybe <$>
    (query conn "SELECT address, nickname FROM nicknames WHERE nickname = ?" (Only nick) :: IO [NickInfo])


lookupAddressesByFuzzyNick :: Text -> Connection -> IO [NickInfo]
lookupAddressesByFuzzyNick nick conn =
    query conn "SELECT address, nickname FROM nicknames WHERE nickname LIKE ? LIMIT 10" (Only $ T.append nick "%") :: IO [NickInfo]
