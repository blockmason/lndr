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

    -- * 'verified_credit' table functions
    , insertCredit
    , insertCredits
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
import qualified Network.Ethereum.Web3.Address as Addr

-- DB configuration

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo { connectUser = "aupiff"
                              , connectDatabase = "lndr" }

-- DB Typeclass instances

instance ToField Address where
    toField = Escape . addrToBS

instance FromField Address where
    fromField f dat = textToAddress <$> fromField f dat

instance FromRow CreditRecord

-- nicknames table manipulations

insertNick :: Address -> Text -> Connection -> IO Int
insertNick addr nick conn = fromIntegral <$>
    execute conn "INSERT INTO nicknames (address, nickname) VALUES (?,?)" (addr, nick)


lookupNick :: Address -> Connection -> IO (Maybe Text)
lookupNick addr conn = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT nickname FROM nicknames WHERE address = ?" (Only addr) :: IO [Only Text])


lookupAddresByNick :: Text -> Connection -> IO [NickInfo]
lookupAddresByNick nick conn = fmap ((`NickInfo` nick) . fromOnly) <$>
    (query conn "SELECT address FROM nicknames WHERE nickname = ?" (Only nick) :: IO [Only Address])

-- friendships table manipulations

addFriends :: Address -> [Address] -> Connection -> IO Int
addFriends addr addresses conn = fromIntegral <$>
    executeMany conn "INSERT INTO friendships (origin, friend) VALUES (?,?)" ((addr,) <$> addresses)


removeFriends :: Address -> [Address] -> Connection -> IO Int
removeFriends addr addresses conn = fromIntegral <$>
    execute conn "DELETE FROM friendships WHERE origin = ? AND friend in ?" (addr, In addresses)


lookupFriends :: Address -> Connection -> IO [Address]
lookupFriends addr conn = fmap fromOnly <$>
    (query conn "SELECT friend FROM friendships WHERE origin = ?" (Only addr) :: IO [Only Address])

lookupFriendsWithNick :: Address -> Connection -> IO [NickInfo]
lookupFriendsWithNick addr conn = fmap (uncurry NickInfo) <$>
    (query conn "SELECT friend, nickname FROM friendships, nicknames WHERE origin = ? AND address = friend" (Only addr) :: IO [(Address, Text)])

-- pending_credits table manipulations

lookupPending :: Text -> Connection -> IO (Maybe CreditRecord)
lookupPending hash conn = listToMaybe <$> query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE hash = ?" (Only hash)


lookupPendingByAddress :: Address -> Connection -> IO [CreditRecord]
lookupPendingByAddress addr conn = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE creditor = ? OR debtor = ?" (addr, addr)


deletePending :: Text -> Connection -> IO Int
deletePending hash conn = fromIntegral <$>
    execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertPending :: CreditRecord -> Connection -> IO Int
insertPending creditRecord conn =
    fromIntegral <$> execute conn "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature) VALUES (?,?,?,?,?,?,?,?)" (creditRecordToPendingTuple creditRecord)


insertCredit :: Text -> Text -> CreditRecord -> Connection -> IO Int
insertCredit creditorSig debtorSig (CreditRecord creditor debtor amount memo submitter nonce hash _) conn =
    fromIntegral <$> execute conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?)" (creditor, debtor, amount, memo, nonce, hash, creditorSig, debtorSig)


insertCredits :: [IssueCreditLog] -> Connection -> IO Int
insertCredits creditLogs conn =
    fromIntegral <$> executeMany conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?) ON CONFLICT (hash) DO NOTHING" (creditLogToCreditTuple <$> creditLogs)


creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text)
creditRecordToPendingTuple (CreditRecord creditor debtor amount memo submitter nonce hash sig) =
    (creditor, debtor, amount, memo, submitter, nonce, hash, sig)


creditLogToCreditTuple :: IssueCreditLog
                       -> (Address, Address, Integer, Text, Integer, Text, Text, Text)
creditLogToCreditTuple cl@(IssueCreditLog ucac creditor debtor amount nonce memo) =
    (creditor, debtor, amount, memo, nonce, hashCreditLog cl, "", "")
