{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db (
    -- * DB Configuration
      dbConfig

    -- * 'nicknames' table functions
    , insertNick
    , lookupNick

    -- * 'friendships' table functions
    , addFriends
    , removeFriends

    -- * 'pending_credit' table functions
    , lookupPending
    , deletePending
    , insertPending
    ) where

import           Database.PostgreSQL.Simple
import           Data.Text
import           Lndr.Types
import           Network.Ethereum.Web3

dbConfig = defaultConnectInfo { connectUser = "aupiff"
                              , connectDatabase = "lndr" }

-- nicknames table manipulations

insertNick :: Connection -> Address -> Text -> IO ()
insertNick = undefined

lookupNick :: Connection -> Address -> IO (Maybe Text)
lookupNick = undefined

-- friendships table manipulations

addFriends :: Connection -> Address -> [Address] -> IO ()
addFriends = undefined

removeFriends :: Connection -> Address -> [Address] -> IO ()
removeFriends = undefined

-- pending_credits table manipulations

lookupPending :: Connection -> Maybe Address -> IO [PendingRecord]
lookupPending = undefined

deletePending :: Connection -> Text -> IO ()
deletePending = undefined

insertPending :: Connection -> PendingRecord -> IO ()
insertPending = undefined
