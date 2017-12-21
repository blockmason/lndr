{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Db (
    -- * 'nicknames' table functions
      insertNick
    , lookupNick
    , lookupAddressByNick
    , lookupAddressesByFuzzyNick

    -- * 'friendships' table functions
    , addFriends
    , removeFriends
    , lookupFriends
    , lookupFriendsWithNick

    -- * 'pending_credit' table functions
    , lookupPending
    , lookupPendingByAddress
    , lookupPendingByAddresses
    , deletePending
    , insertPending

    -- * 'verified_credit' table functions
    , insertCredit
    , insertCredits
    , allCredits
    , lookupCreditByAddress
    , counterpartiesByAddress
    , lookupCreditByHash
    , userBalance
    , twoPartyBalance
    , twoPartyNonce

    -- * 'push_data' table functions
    , insertPushDatum
    , lookupPushDatumByAddress
    ) where


import           Data.Maybe (listToMaybe)
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Lndr.EthInterface
import           Lndr.Types
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

-- nicknames table manipulations

insertNick :: Address -> Text -> Connection -> IO Int
insertNick addr nick conn = fromIntegral <$>
    execute conn "INSERT INTO nicknames (address, nickname) VALUES (?,?) ON CONFLICT (address) DO UPDATE SET nickname = EXCLUDED.nickname" (addr, nick)


lookupNick :: Address -> Connection -> IO (Maybe Text)
lookupNick addr conn = listToMaybe . fmap fromOnly <$>
    (query conn "SELECT nickname FROM nicknames WHERE address = ?" (Only addr) :: IO [Only Text])


-- TODO update this to return a maybe
lookupAddressByNick :: Text -> Connection -> IO (Maybe NickInfo)
lookupAddressByNick nick conn = listToMaybe <$>
    (query conn "SELECT address, nickname FROM nicknames WHERE nickname = ?" (Only nick) :: IO [NickInfo])


lookupAddressesByFuzzyNick :: Text -> Connection -> IO [NickInfo]
lookupAddressesByFuzzyNick nick conn =
    query conn "SELECT address, nickname FROM nicknames WHERE nickname LIKE ? LIMIT 10" (Only $ T.append nick "%") :: IO [NickInfo]


-- friendships table manipulations

addFriends :: Address -> [Address] -> Connection -> IO Int
addFriends addr addresses conn = fromIntegral <$>
    executeMany conn "INSERT INTO friendships (origin, friend) VALUES (?,?) ON CONFLICT ON CONSTRAINT friendships_origin_friend_key DO NOTHING" ((addr,) <$> addresses)


removeFriends :: Address -> [Address] -> Connection -> IO Int
removeFriends addr addresses conn = fromIntegral <$>
    execute conn "DELETE FROM friendships WHERE origin = ? AND friend in ?" (addr, In addresses)


lookupFriends :: Address -> Connection -> IO [Address]
lookupFriends addr conn = fmap fromOnly <$>
    (query conn "SELECT friend FROM friendships WHERE origin = ?" (Only addr) :: IO [Only Address])

lookupFriendsWithNick :: Address -> Connection -> IO [NickInfo]
lookupFriendsWithNick addr conn =
    query conn "SELECT friend, nickname FROM friendships, nicknames WHERE origin = ? AND address = friend" (Only addr) :: IO [NickInfo]

-- pending_credits table manipulations

lookupPending :: Text -> Connection -> IO (Maybe CreditRecord)
lookupPending hash conn = listToMaybe <$> query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE hash = ?" (Only hash)


lookupPendingByAddress :: Address -> Connection -> IO [CreditRecord]
lookupPendingByAddress addr conn = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE creditor = ? OR debtor = ?" (addr, addr)


lookupPendingByAddresses :: Address -> Address -> Connection -> IO [CreditRecord]
lookupPendingByAddresses p1 p2 conn = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (p1, p2, p2, p1)


deletePending :: Text -> Connection -> IO Int
deletePending hash conn = fromIntegral <$>
    execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertPending :: CreditRecord -> Connection -> IO Int
insertPending creditRecord conn =
    fromIntegral <$> execute conn "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature) VALUES (?,?,?,?,?,?,?,?)" (creditRecordToPendingTuple creditRecord)


insertCredit :: Text -> Text -> CreditRecord -> Connection -> IO Int
insertCredit creditorSig debtorSig (CreditRecord creditor debtor amount memo _ nonce hash _) conn =
    fromIntegral <$> execute conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?)" (creditor, debtor, amount, memo, nonce, hash, creditorSig, debtorSig)


insertCredits :: [IssueCreditLog] -> Connection -> IO Int
insertCredits creditLogs conn =
    fromIntegral <$> executeMany conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?) ON CONFLICT (hash) DO NOTHING" (creditLogToCreditTuple <$> creditLogs)


-- TODO fix this creditor, creditor repetition
allCredits :: Connection -> IO [IssueCreditLog]
allCredits conn = query conn "SELECT creditor, creditor, debtor, amount, nonce, memo FROM verified_credits" ()

-- TODO fix this creditor, creditor repetition
lookupCreditByAddress :: Address -> Connection -> IO [IssueCreditLog]
lookupCreditByAddress addr conn = query conn "SELECT creditor, creditor, debtor, amount, nonce, memo FROM verified_credits WHERE creditor = ? OR debtor = ?" (addr, addr)


counterpartiesByAddress :: Address -> Connection -> IO [Address]
counterpartiesByAddress addr conn = fmap fromOnly <$>
    query conn "SELECT creditor FROM verified_credits WHERE debtor = ? UNION SELECT debtor FROM verified_credits WHERE creditor = ?" (addr, addr)


-- TODO fix this creditor, creditor repetition
lookupCreditByHash :: Text -> Connection -> IO (Maybe (CreditRecord, Text, Text))
lookupCreditByHash hash conn = (fmap process . listToMaybe) <$> query conn "SELECT creditor, debtor, amount, nonce, memo, creditor_signature, debtor_signature FROM verified_credits WHERE hash = ?" (Only hash)
    where process (creditor, debtor, amount, nonce, memo, sig1, sig2) = ( CreditRecord creditor debtor
                                                                                       amount memo
                                                                                       creditor nonce hash sig1
                                                                        , sig1
                                                                        , sig2
                                                                        )


userBalance :: Address -> Connection -> IO Integer
userBalance addr conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE debtor = ?)" (addr, addr) :: IO [Only Scientific]
    return . floor $ balance


twoPartyBalance :: Address -> Address -> Connection -> IO Integer
twoPartyBalance addr counterparty conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ?)" (addr, counterparty, counterparty, addr) :: IO [Only Scientific]
    return . floor $ balance


twoPartyNonce :: Address -> Address -> Connection -> IO Nonce
twoPartyNonce addr counterparty conn = do
    [Only nonce] <- query conn "SELECT COALESCE(MAX(nonce) + 1, 0) FROM verified_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (addr, counterparty, counterparty, addr) :: IO [Only Scientific]
    return . Nonce . floor $ nonce


insertPushDatum :: Address -> Text -> Text -> Connection -> IO Int
insertPushDatum addr channelID platform conn = fromIntegral <$>
    execute conn "INSERT INTO push_data (address, channel_id, platform) VALUES (?,?,?) ON CONFLICT (address) DO UPDATE SET (channel_id, platform) = (EXCLUDED.channel_id, EXCLUDED.platform)" (addr, channelID, platform)


lookupPushDatumByAddress :: Address -> Connection -> IO (Maybe (Text, DevicePlatform))
lookupPushDatumByAddress addr conn = listToMaybe <$> query conn "SELECT channel_id, platform FROM push_data WHERE address = ?" (Only addr)

creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text)
creditRecordToPendingTuple (CreditRecord creditor debtor amount memo submitter nonce hash sig) =
    (creditor, debtor, amount, memo, submitter, nonce, hash, sig)


creditLogToCreditTuple :: IssueCreditLog
                       -> (Address, Address, Integer, Text, Integer, Text, Text, Text)
creditLogToCreditTuple cl@(IssueCreditLog _ creditor debtor amount nonce memo) =
    (creditor, debtor, amount, memo, nonce, hashCreditLog cl, "", "")
