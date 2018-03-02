{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.VerifiedCredits where

import           Control.Arrow (first)
import           Control.Monad
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Scientific
import           Database.PostgreSQL.Simple
import           Lndr.Types
import           Lndr.Db.Types
import           Lndr.Util
import           Network.Ethereum.Web3

insertCredit :: BilateralCreditRecord -> Connection -> IO Int
insertCredit bilateralCreditRecord conn = fromIntegral <$> execute conn sql bilateralCreditRecord
    where sql = "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature, ucac, submitter) VALUES (?,?,?,?,?,?,?,?,?,?)"


insertCredits :: [IssueCreditLog] -> Connection -> IO Int
insertCredits creditLogs conn =
    fromIntegral <$> executeMany conn sql creditLogs
    where sql = "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature, ucac, submitter) VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT (hash) DO NOTHING"


allCredits :: Connection -> IO [IssueCreditLog]
allCredits conn = query_ conn "SELECT ucac, creditor, debtor, amount, nonce, memo FROM verified_credits"


lookupCreditByAddress :: Address -> Connection -> IO [IssueCreditLog]
lookupCreditByAddress addr conn = query conn "SELECT ucac, creditor, debtor, verified_credits.amount, nonce, memo FROM verified_credits LEFT JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND (settlements.hash IS NULL OR settlements.verified = TRUE) ORDER BY created_at DESC" (addr, addr)


deleteExpiredSettlementsAndAssociatedCredits :: Connection -> IO ()
deleteExpiredSettlementsAndAssociatedCredits conn = do
    begin conn
    hashes <- fmap fromOnly <$> query_ conn "SELECT hash FROM settlements WHERE created_at < now() - interval '2 days' AND verified = FALSE" :: IO [Text]
    execute conn "DELETE FROM verified_credits WHERE hash IN ?" (Only $ In hashes)
    execute conn "DELETE FROM pending_credits WHERE hash IN ?" (Only $ In hashes)
    execute conn "DELETE FROM settlements WHERE hash IN ?" (Only $ In hashes)
    commit conn


settlementCreditsToVerify :: Connection -> IO [Text]
settlementCreditsToVerify conn = fmap fromOnly <$> query_ conn "SELECT hash FROM settlements WHERE tx_hash IS NOT NULL AND verified = FALSE"


txHashByCreditHash :: Text -> Connection -> IO (Maybe Text)
txHashByCreditHash creditHash conn = fmap fromOnly . join . listToMaybe <$> query conn "SELECT tx_hash FROM settlements WHERE hash = ?" (Only creditHash)


updateSettlementTxHash :: Text -> Text -> Connection -> IO Int
updateSettlementTxHash hash txHash conn = fromIntegral <$> execute conn "UPDATE settlements SET  tx_hash = ? WHERE hash = ?" (txHash, hash)


lookupSettlementCreditByAddress :: Address -> Connection -> IO [BilateralCreditRecord]
lookupSettlementCreditByAddress addr conn = query conn "SELECT creditor, debtor, verified_credits.amount, memo, submitter, nonce, verified_credits.hash, ucac, creditor_signature, debtor_signature, settlements.amount, settlements.currency, settlements.blocknumber, settlements.tx_hash FROM verified_credits JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND verified = FALSE" (addr, addr)


counterpartiesByAddress :: Address -> Connection -> IO [Address]
counterpartiesByAddress addr conn = fmap fromOnly <$>
    query conn "SELECT creditor FROM verified_credits WHERE debtor = ? UNION SELECT debtor FROM verified_credits WHERE creditor = ?" (addr, addr)


lookupCreditByHash :: Text -> Connection -> IO (Maybe BilateralCreditRecord)
lookupCreditByHash hash conn = listToMaybe <$> query conn sql (Only hash)
    where sql = "SELECT creditor, debtor, verified_credits.amount, memo, submitter, nonce, verified_credits.hash, ucac, creditor_signature, debtor_signature, settlements.amount, settlements.currency, settlements.blocknumber, settlements.tx_hash FROM verified_credits JOIN settlements ON verified_credits.hash = settlements.hash WHERE verified_credits.hash = ?"


-- Flips verified bit on once a settlement payment has been confirmed
verifyCreditByHash :: Text -> Connection -> IO Int
verifyCreditByHash hash conn = fromIntegral <$> execute conn "UPDATE settlements SET verified = TRUE WHERE hash = ?" (Only hash)


userBalance :: Address -> Address -> Connection -> IO Integer
userBalance addr ucac conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND ucac = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE debtor = ? AND ucac = ?)" (addr, ucac, addr, ucac) :: IO [Only Scientific]
    return . floor $ balance


twoPartyBalance :: Address -> Address -> Address -> Connection -> IO Integer
twoPartyBalance addr counterparty ucac conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ? AND ucac = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ? AND ucac = ?)" (addr, counterparty, ucac, counterparty, addr, ucac) :: IO [Only Scientific]
    return . floor $ balance


twoPartyNonce :: Address -> Address -> Connection -> IO Nonce
twoPartyNonce addr counterparty conn = do
    [Only nonce] <- query conn "SELECT COALESCE(MAX(nonce) + 1, 0) FROM verified_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (addr, counterparty, counterparty, addr) :: IO [Only Scientific]
    return . Nonce . floor $ nonce
