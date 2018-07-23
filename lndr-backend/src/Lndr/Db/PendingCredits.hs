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
lookupPending hash conn = listToMaybe <$> query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac, settlements.currency, settlements.amount, settlements.blocknumber FROM pending_credits LEFT JOIN settlements USING(hash) WHERE pending_credits.hash = ?" (Only hash)

-- Boolean parameter determines if search is through settlement records or
-- non-settlement records
lookupPendingByAddress :: Address -> Bool -> Connection -> IO [CreditRecord]
lookupPendingByAddress addr True conn = query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac, settlements.currency, settlements.amount, settlements.blocknumber FROM pending_credits JOIN settlements USING(hash) WHERE (creditor = ? OR debtor = ?)" (addr, addr)
lookupPendingByAddress addr False conn = query conn "SELECT creditor, debtor, pending_credits.amount, memo, submitter, nonce, pending_credits.hash, signature, ucac, settlement_currency FROM pending_credits LEFT JOIN settlements USING(hash) WHERE (creditor = ? OR debtor = ?) AND settlements.hash IS NULL" (addr, addr)


lookupPendingSettlementByAddresses :: Address -> Address -> Connection -> IO [Only Text]
lookupPendingSettlementByAddresses p1 p2 conn = query conn "SELECT verified_credits.hash FROM verified_credits JOIN settlements USING(hash) WHERE ((creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)) AND settlements.verified = FALSE" (p1, p2, p2, p1)


lookupPendingByAddresses :: Address -> Address -> Connection -> IO [CreditRecord]
lookupPendingByAddresses p1 p2 conn = query conn "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, signature, ucac, settlement_currency FROM pending_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (p1, p2, p2, p1)


deletePending :: Text -> Bool -> Connection -> IO Int
deletePending hash rejection conn = do
    when rejection . void $ execute conn "DELETE FROM settlements WHERE hash = ?" (Only hash)
    fromIntegral <$> execute conn "DELETE FROM pending_credits WHERE hash = ?" (Only hash)


insertSettlementData :: CreditRecord -> Connection -> IO Int
insertSettlementData (CreditRecord _ _ _ _ _ _ hash _ _ (Just currency) (Just amount) (Just blocknumber)) conn =
    fromIntegral <$> execute conn "INSERT INTO settlements (hash, amount, currency, blocknumber, verified) VALUES (?,?,?,?,FALSE)" (hash, amount, currency, blocknumber)
insertSettlementData _ _ = return 0


insertPending :: CreditRecord -> Connection -> IO Int
insertPending creditRecord conn = do
    insertSettlementData creditRecord conn
    fromIntegral <$> execute conn sql (creditRecordToPendingTuple creditRecord)
    where sql = "INSERT INTO pending_credits (creditor, debtor, amount, memo, submitter, nonce, hash, signature, ucac, settlement_currency) VALUES (?,?,?,?,?,?,?,?,?,?)"

-- utility functions

creditRecordToPendingTuple :: CreditRecord
                           -> (Address, Address, Integer, Text, Address, Integer, Text, Text, Address, Maybe Text)
creditRecordToPendingTuple creditRecord = ( creditor creditRecord
                                          , debtor creditRecord
                                          , amount creditRecord
                                          , memo creditRecord
                                          , submitter creditRecord
                                          , nonce creditRecord
                                          , hash creditRecord
                                          , signature creditRecord
                                          , ucac creditRecord
                                          , settlementCurrency creditRecord
                                          )


insertPayPalRequest :: PayPalRequest -> Connection -> IO Int
insertPayPalRequest r@(PayPalRequest friend requestor sign) conn = do
    fromIntegral <$> execute conn "INSERT INTO paypal_requests (friend, requestor) VALUES (?,?)" (friend, requestor)


deletePayPalRequest :: PayPalRequest -> Connection -> IO Int
deletePayPalRequest r@(PayPalRequest friend requestor sign) conn = do
    fromIntegral <$> execute conn "DELETE FROM paypal_requests WHERE (friend = ? AND requestor = ?)" (friend, requestor)


lookupPayPalRequestsByAddress :: Address -> Connection -> IO [PayPalRequestPair]
lookupPayPalRequestsByAddress userAddr conn = do
    query conn "SELECT paypal_requests.friend, paypal_requests.requestor, friends.nickname, requestors.nickname FROM paypal_requests LEFT JOIN nicknames requestors ON requestors.address = requestor LEFT JOIN nicknames friends ON friends.address = friend WHERE (friend = ? OR requestor = ?)" (userAddr, userAddr) :: IO [PayPalRequestPair]
