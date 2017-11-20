{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Db (
    -- * DB Configuration
      dbConfig

    -- * 'nicknames' table functions
    , insertNick
    , lookupNick

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
    toField addr = Plain . byteString . addrToBS  $ addr

-- instance FromField Address where
--     fromField f dat = textToAddress <$> field f dat

-- nicknames table manipulations

insertNick :: Connection -> Address -> Text -> IO Int
insertNick conn addr nick = fmap fromIntegral $
    execute conn "insert into nicknames (address, nickname) values (?,?)" (addr, nick)


lookupNick :: Connection -> Address -> IO (Maybe Text)
lookupNick conn addr = listToMaybe . fmap T.pack <$>
    query conn "select nickname from nicknames where address = ?" (Only addr)

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

lookupPending :: Connection -> Maybe Address -> IO [PendingRecord]
lookupPending = undefined


deletePending :: Connection -> Text -> IO ()
deletePending = undefined


insertPending :: Connection -> PendingRecord -> IO ()
insertPending = undefined
