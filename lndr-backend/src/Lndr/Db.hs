{-# LANGUAGE OverloadedStrings #-}

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
    , lookupSettlementCreditByAddress
    , counterpartiesByAddress
    , lookupCreditByHash
    , lookupPendingSettlementByAddresses
    , verifyCreditByHash
    , userBalance
    , twoPartyBalance
    , twoPartyNonce

    -- * 'push_data' table functions
    , insertPushDatum
    , lookupPushDatumByAddress
    ) where


import           Control.Monad (when, void)
import           Data.Maybe (listToMaybe)
import           Data.Scientific
import           Data.Text (Text)
import           Data.Foldable
import           Database.PostgreSQL.Simple
import           Lndr.Db.Nicknames
import           Lndr.Db.Friendships
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3

-- pending_credits table manipulations

lookupPending :: Text -> Connection -> IO (Maybe CreditRecord)
lookupPending hash conn = (fmap creditRowToCreditRecord . listToMaybe) <$> query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE hash = ?" (Only hash)


-- Boolean parameter determines if search is through settlement records or
-- non-settlement records
lookupPendingByAddress :: Address -> Bool -> Connection -> IO [CreditRecord]
lookupPendingByAddress addr True conn = fmap settlementCreditRowToCreditRecord <$> query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, settlements.amount, settlements.currency, settlements.blocknumber FROM pending_credits JOIN settlements ON pending_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?)" (addr, addr)
lookupPendingByAddress addr False conn = fmap creditRowToCreditRecord <$> query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature FROM pending_credits LEFT JOIN settlements ON pending_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND settlements.hash IS NULL" (addr, addr)


lookupPendingSettlementByAddresses :: Address -> Address -> Connection -> IO [Only Text]
lookupPendingSettlementByAddresses p1 p2 conn = query conn "SELECT verified_credits.hash FROM verified_credits JOIN settlements ON settlements.hash = verified_credits.hash WHERE ((creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)) AND settlements.verified = FALSE" (p1, p2, p2, p1)


lookupPendingByAddresses :: Address -> Address -> Connection -> IO [CreditRecord]
lookupPendingByAddresses p1 p2 conn = fmap creditRowToCreditRecord <$> query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature FROM pending_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (p1, p2, p2, p1)


deletePending :: Text -> Bool -> Connection -> IO Int
deletePending hash rejection conn = do
    when rejection . void $ execute conn "DELETE FROM settlements WHERE hash = ?" (Only hash)
    fromIntegral <$> execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertSettlementData :: Text -> SettlementData -> Connection -> IO Int
insertSettlementData hash (SettlementData amount currency blocknumber) conn =
    fromIntegral <$> execute conn "INSERT INTO settlements (hash, amount, currency, blocknumber, verified) VALUES (?,?,?,?,FALSE)" (hash, amount, currency, blocknumber)


insertPending :: CreditRecord -> Maybe SettlementData -> Connection -> IO Int
insertPending creditRecord settlementDataM conn = do
    traverse_ (flip (insertSettlementData (hash creditRecord)) conn) settlementDataM
    fromIntegral <$> execute conn "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature) VALUES (?,?,?,?,?,?,?,?)" (creditRecordToPendingTuple creditRecord)


insertCredit :: Text -> Text -> CreditRecord -> Connection -> IO Int
insertCredit creditorSig debtorSig (CreditRecord creditor debtor amount memo _ nonce hash _ _ _ _) conn =
    fromIntegral <$> execute conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?)" (creditor, debtor, amount, memo, nonce, hash, creditorSig, debtorSig)


insertCredits :: [IssueCreditLog] -> Connection -> IO Int
insertCredits creditLogs conn =
    fromIntegral <$> executeMany conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature) VALUES (?,?,?,?,?,?,?,?) ON CONFLICT (hash) DO NOTHING" (creditLogToCreditTuple <$> creditLogs)


-- TODO fix this creditor, creditor repetition
allCredits :: Connection -> IO [IssueCreditLog]
allCredits conn = query conn "SELECT creditor, creditor, debtor, amount, nonce, memo FROM verified_credits" ()

-- TODO fix this creditor, creditor repetition
-- Boolean parameter determines if search is through settlement records or
-- non-settlement records
lookupCreditByAddress :: Address -> Connection -> IO [IssueCreditLog]
lookupCreditByAddress addr conn = query conn "SELECT creditor, creditor, debtor, verified_credits.amount, nonce, memo FROM verified_credits LEFT JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND settlements.hash IS NULL" (addr, addr)


lookupSettlementCreditByAddress :: Address -> Connection -> IO [CreditRecord]
lookupSettlementCreditByAddress addr conn = fmap settlementCreditRowToCreditRecord <$> query conn "SELECT creditor, debtor, verified_credits.amount, memo, creditor, nonce, verified_credits.hash, settlements.amount, settlements.currency, settlements.blocknumber FROM verified_credits JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND verified = FALSE" (addr, addr)


counterpartiesByAddress :: Address -> Connection -> IO [Address]
counterpartiesByAddress addr conn = fmap fromOnly <$>
    query conn "SELECT creditor FROM verified_credits WHERE debtor = ? UNION SELECT debtor FROM verified_credits WHERE creditor = ?" (addr, addr)


lookupCreditByHash :: Text -> Connection -> IO (Maybe (CreditRecord, Text, Text))
lookupCreditByHash hash conn = do
        settlementAmount <- fmap ((floor :: Rational -> Integer) . fromOnly) . listToMaybe <$> query conn "SELECT amount FROM settlements WHERE hash = ?" (Only hash)
        let process (creditor, debtor, amount, nonce, memo, sig1, sig2) = ( CreditRecord creditor debtor
                                                                                         amount memo
                                                                                         creditor nonce hash sig1
                                                                                         settlementAmount
                                                                                         Nothing
                                                                                         Nothing
                                                                          , sig1
                                                                          , sig2
                                                                          )
        (fmap process . listToMaybe) <$> query conn "SELECT creditor, debtor, amount, nonce, memo, creditor_signature, debtor_signature FROM verified_credits WHERE hash = ?" (Only hash)

-- Flips verified bit on once a settlement payment has been confirmed
verifyCreditByHash :: Text -> Connection -> IO Int
verifyCreditByHash hash conn = fromIntegral <$> execute conn "UPDATE settlements SET verified = TRUE WHERE hash = ?" (Only hash)

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

-- 'push_data' table functions

insertPushDatum :: Address -> Text -> Text -> Connection -> IO Int
insertPushDatum addr channelID platform conn = fromIntegral <$>
    execute conn "INSERT INTO push_data (address, channel_id, platform) VALUES (?,?,?) ON CONFLICT (address) DO UPDATE SET (channel_id, platform) = (EXCLUDED.channel_id, EXCLUDED.platform)" (addr, channelID, platform)


lookupPushDatumByAddress :: Address -> Connection -> IO (Maybe (Text, DevicePlatform))
lookupPushDatumByAddress addr conn = listToMaybe <$> query conn "SELECT channel_id, platform FROM push_data WHERE address = ?" (Only addr)

-- utility functions

creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text)
creditRecordToPendingTuple (CreditRecord creditor debtor amount memo submitter nonce hash sig _ _ _) =
    (creditor, debtor, amount, memo, submitter, nonce, hash, sig)


creditLogToCreditTuple :: IssueCreditLog
                       -> (Address, Address, Integer, Text, Integer, Text, Text, Text)
creditLogToCreditTuple cl@(IssueCreditLog _ creditor debtor amount nonce memo) =
    (creditor, debtor, amount, memo, nonce, hashCreditLog cl, "", "")
