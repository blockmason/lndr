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
    , lookupFriendsWithNick

    -- * 'pending_credit' table functions
    , lookupPending
    , lookupPendingByAddress
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

instance FromRow CreditRecord

-- nicknames table manipulations

insertNick :: Connection -> Address -> Text -> IO Int
insertNick conn addr nick = fmap fromIntegral $
    execute conn "INSERT INTO nicknames (address, nickname) VALUES (?,?)" (addr, nick)


lookupNick :: Connection -> Address -> IO (Maybe Text)
lookupNick conn addr = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT nickname FROM nicknames WHERE address = ?" (Only addr) :: IO [Only Text])


lookupAddresByNick :: Connection -> Text -> IO [NickInfo]
lookupAddresByNick conn nick = fmap ((\x -> NickInfo x nick) . fromOnly) <$>
    (query conn "SELECT address FROM nicknames WHERE nickname = ?" (Only nick) :: IO [Only Address])

-- friendships table manipulations

addFriends :: Connection -> Address -> [Address] -> IO Int
addFriends conn addr addresses = fromIntegral <$>
    executeMany conn "INSERT INTO friendships (origin, friend) VALUES (?,?)" ((addr,) <$> addresses)


removeFriends :: Connection -> Address -> [Address] -> IO Int
removeFriends conn addr addresses = fromIntegral <$>
    execute conn "DELETE FROM friendships WHERE origin = ? AND friend in ?" (addr, In addresses)


lookupFriends :: Connection -> Address -> IO [Address]
lookupFriends conn addr = fmap fromOnly <$>
    (query conn "SELECT friend FROM friendships WHERE origin = ?" (Only addr) :: IO [Only Address])

lookupFriendsWithNick :: Connection -> Address -> IO [NickInfo]
lookupFriendsWithNick conn addr = fmap (uncurry NickInfo) <$>
    (query conn "SELECT friend, nickname FROM friendships, nicknames WHERE origin = ? AND address = friend" (Only addr) :: IO [(Address, Text)])

-- pending_credits table manipulations

lookupPending :: Connection -> Text -> IO (Maybe CreditRecord)
lookupPending conn hash = listToMaybe <$> query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE hash = ?" (Only hash)


lookupPendingByAddress :: Connection -> Address -> IO [CreditRecord]
lookupPendingByAddress conn addr = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE creditor = ? OR debtor = ?" (addr, addr)


deletePending :: Connection -> Text -> IO Int
deletePending conn hash = fromIntegral <$>
    execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertPending :: Connection -> CreditRecord -> IO Int
insertPending conn (CreditRecord creditor debtor amount memo submitter nonce hash sig) =
    fmap fromIntegral $ execute conn "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature) VALUES (?,?,?,?,?,?,?,?)" (creditor, debtor, amount, memo, submitter, nonce, hash, sig)
