{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Db (
    -- * DB Configuration
      dbConfig

    -- * 'nicknames' table functions
    , insertNick
    , lookupNick
    , lookupAddresByNick

    -- * 'friendships' table functions
    , addFriends
    , removeFriends
    , lookupFriends

    -- * 'pending_credit' table functions
    , lookupPending
    , deletePending
    , insertPending
    ) where

import           Data.ByteString.Builder (byteString)
import           Data.Maybe (listToMaybe)
import           Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Lndr.EthInterface
import           Lndr.Types
import           Network.Ethereum.Web3

-- DB configuration

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo { connectUser = "aupiff"
                              , connectDatabase = "lndr" }

-- DB Typeclass instances

instance ToField Address where
    toField addr = Escape . addrToBS  $ addr

instance FromField Address where
    fromField f dat = textToAddress <$> fromField f dat

instance FromRow PendingRecordFlat

-- nicknames table manipulations

insertNick :: Connection -> Address -> Text -> IO Int
insertNick conn addr nick = fmap fromIntegral $
    execute conn "insert into nicknames (address, nickname) values (?,?)" (addr, nick)


lookupNick :: Connection -> Address -> IO (Maybe Text)
lookupNick conn addr = listToMaybe . fmap fromOnly <$>
    (query conn "select nickname from nicknames where address = ?" (Only addr) :: IO [Only Text])


lookupAddresByNick :: Connection -> Text -> IO [NickInfo]
lookupAddresByNick conn nick = fmap ((\x -> NickInfo x nick) . fromOnly) <$>
    (query conn "select address from nicknames where nickname = ?" (Only nick) :: IO [Only Address])

-- friendships table manipulations

addFriends :: Connection -> Address -> [Address] -> IO Int
addFriends conn addr addresses = fromIntegral <$>
    executeMany conn "insert into friendships (origin, friend) values (?,?)" ((addr,) <$> addresses)


removeFriends :: Connection -> Address -> [Address] -> IO Int
removeFriends conn addr addresses = fromIntegral <$>
    executeMany conn "delete from friendships where origin = ?, friend = ?" ((addr,) <$> addresses)


lookupFriends :: Connection -> Address -> IO [Address]
lookupFriends conn addr = fmap (textToAddress . T.pack) <$>
    query conn "SELECT friend FROM friendships WHERE origin = ?" (Only addr)

-- pending_credits table manipulations

lookupPending :: Connection -> Text -> IO [PendingRecordFlat]
lookupPending conn hash = query conn "SELECT friend FROM pending_credits WHERE hash = ?" (Only hash)


deletePending :: Connection -> Text -> IO Int
deletePending conn hash = fromIntegral <$>
    execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertPending :: Connection -> PendingRecord -> IO ()
insertPending = undefined
