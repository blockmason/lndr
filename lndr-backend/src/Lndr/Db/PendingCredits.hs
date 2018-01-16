{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.PendingCredits where

import           Control.Monad (when, void)
import           Data.Foldable
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Lndr.Db.Types
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3

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

-- utility functions

creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text)
creditRecordToPendingTuple (CreditRecord creditor debtor amount memo submitter nonce hash sig _ _ _) =
    (creditor, debtor, amount, memo, submitter, nonce, hash, sig)


creditLogToCreditTuple :: IssueCreditLog
                       -> (Address, Address, Integer, Text, Integer, Text, Text, Text)
creditLogToCreditTuple cl@(IssueCreditLog _ creditor debtor amount nonce memo) =
    (creditor, debtor, amount, memo, nonce, hashCreditLog cl, "", "")
