{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Nicknames where

import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Lndr.Db.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Text.EmailAddress
import qualified Text.EmailAddress as Email


insertNick :: Address -> Text -> Connection -> IO Int
insertNick addr nick conn = fromIntegral <$>
    execute conn "INSERT INTO nicknames (address, nickname) VALUES (?,?) ON CONFLICT (address) DO UPDATE SET nickname = EXCLUDED.nickname" (addr, nick)


insertEmail :: Address -> Text -> Connection -> IO Int
insertEmail addr email conn = fromIntegral <$>
    execute conn "INSERT INTO nicknames (address, email) VALUES (?,?) ON CONFLICT (address) DO UPDATE SET email = EXCLUDED.email" (addr, email)


lookupNick :: Address -> Connection -> IO (Maybe Text)
lookupNick addr conn = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT nickname FROM nicknames WHERE address = ?" (Only addr) :: IO [Only Text])


lookupEmail :: Address -> Connection -> IO (Maybe EmailAddress)
lookupEmail addr conn = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT email FROM nicknames WHERE address = ?" (Only addr) :: IO [Only EmailAddress])


lookupAddressByNick :: Text -> Connection -> IO (Maybe UserInfo)
lookupAddressByNick nick conn = listToMaybe <$>
    (query conn "SELECT address, nickname FROM nicknames WHERE nickname = ?" (Only nick) :: IO [UserInfo])

lookupAddressByEmail :: EmailAddress -> Connection -> IO (Maybe UserInfo)
lookupAddressByEmail email conn = listToMaybe <$>
    (query conn "SELECT address, nickname FROM nicknames WHERE email = ?" (Only $ Email.toText email) :: IO [UserInfo])


lookupAddressesByFuzzyNick :: Text -> Connection -> IO [UserInfo]
lookupAddressesByFuzzyNick nick conn =
    query conn "SELECT address, nickname FROM nicknames WHERE nickname LIKE ? LIMIT 10" (Only $ T.append nick "%") :: IO [UserInfo]
