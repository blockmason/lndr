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

-- TODO VERIFY THAT JOIN is appopriate here
lookupPending :: Text -> Connection -> IO (Maybe CreditRecord)
lookupPending hash conn = listToMaybe <$> query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac, settlements.amount, settlements.currency, settlements.blocknumber FROM pending_credits LEFT JOIN settlements ON pending_credits.hash = settlements.hash WHERE pending_credits.hash = ?" (Only hash)

-- Boolean parameter determines if search is through settlement records or
-- non-settlement records
lookupPendingByAddress :: Address -> Bool -> Connection -> IO [CreditRecord]
lookupPendingByAddress addr True conn = query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac, settlements.amount, settlements.currency, settlements.blocknumber FROM pending_credits JOIN settlements ON pending_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?)" (addr, addr)
lookupPendingByAddress addr False conn = query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac FROM pending_credits LEFT JOIN settlements ON pending_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND settlements.hash IS NULL" (addr, addr)


lookupPendingSettlementByAddresses :: Address -> Address -> Connection -> IO [Only Text]
lookupPendingSettlementByAddresses p1 p2 conn = query conn "SELECT verified_credits.hash FROM verified_credits JOIN settlements ON settlements.hash = verified_credits.hash WHERE ((creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)) AND settlements.verified = FALSE" (p1, p2, p2, p1)


lookupPendingByAddresses :: Address -> Address -> Connection -> IO [CreditRecord]
lookupPendingByAddresses p1 p2 conn = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature, ucac FROM pending_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (p1, p2, p2, p1)


deletePending :: Text -> Bool -> Connection -> IO Int
deletePending hash rejection conn = do
    when rejection . void $ execute conn "DELETE FROM settlements WHERE hash = ?" (Only hash)
    fromIntegral <$> execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertSettlementData :: CreditRecord -> Connection -> IO Int
insertSettlementData (CreditRecord _ _ _ _ _ _ hash _ _ (Just amount) (Just currency) (Just blocknumber)) conn =
    fromIntegral <$> execute conn "INSERT INTO settlements (hash, amount, currency, blocknumber, verified) VALUES (?,?,?,?,FALSE)" (hash, amount, currency, blocknumber)
insertSettlementData _ _ = return 0


insertPending :: CreditRecord -> Connection -> IO Int
insertPending creditRecord conn = do
    insertSettlementData creditRecord conn
    fromIntegral <$> execute conn sql (creditRecordToPendingTuple creditRecord)
    where sql = "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature, ucac) VALUES (?,?,?,?,?,?,?,?,?)"

-- utility functions

creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text, Address)
creditRecordToPendingTuple creditRecord = ( creditor creditRecord
                                          , debtor creditRecord
                                          , amount creditRecord
                                          , memo creditRecord
                                          , submitter creditRecord
                                          , nonce creditRecord
                                          , hash creditRecord
                                          , signature creditRecord
                                          , ucac creditRecord
                                          )
