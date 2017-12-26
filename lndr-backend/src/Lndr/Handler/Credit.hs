{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Credit where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Pool (withResource)
import qualified Data.Text as T
import qualified Lndr.Db as Db
import           Lndr.EthInterface
import           Lndr.Handler.Types
import           Lndr.Notifications
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Util
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant


rejectHandler :: RejectRecord -> LndrHandler NoContent
rejectHandler(RejectRecord sig hash) = do
    pool <- dbConnectionPool <$> ask
    pendingRecordM <- liftIO . withResource pool $ Db.lookupPending hash
    let hashNotFound = throwError $ err404 { errBody = "credit hash does not refer to pending record" }
    (CreditRecord creditor debtor _ _ _ _ _ _) <- maybe hashNotFound pure pendingRecordM
    -- recover address from sig
    let signer = EU.ecrecover (stripHexPrefix sig) hash
    case signer of
        Left _ -> throwError $ err400 { errBody = "unable to recover addr from sig" }
        Right addr -> if textToAddress addr == debtor || textToAddress addr == creditor
                            then do liftIO . withResource pool $ Db.deletePending hash
                                    return NoContent
                            else throwError $ err400 { errBody = "bad rejection sig" }


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler Nothing = do
    configTVar <- serverConfig <$> ask
    config <- liftIO . atomically $ readTVar configTVar
    lndrWeb3 (lndrLogs config Nothing Nothing)
transactionsHandler (Just addr) = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.lookupCreditByAddress addr


counterpartiesHandler :: Address -> LndrHandler [Address]
counterpartiesHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.counterpartiesByAddress addr


balanceHandler :: Address -> LndrHandler Integer
balanceHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.userBalance addr


twoPartyBalanceHandler :: Address -> Address -> LndrHandler Integer
twoPartyBalanceHandler p1 p2 = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.twoPartyBalance p1 p2


pendingHandler :: Address -> LndrHandler [CreditRecord]
pendingHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.lookupPendingByAddress addr


lendHandler :: CreditRecord -> LndrHandler NoContent
lendHandler creditRecord = submitHandler (creditor creditRecord) creditRecord


borrowHandler :: CreditRecord -> LndrHandler NoContent
borrowHandler creditRecord = submitHandler (debtor creditRecord) creditRecord


submitHandler :: Address -> CreditRecord -> LndrHandler NoContent
submitHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ memo _ _ _ sig) = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    nonce <- liftIO . withResource pool $ Db.twoPartyNonce creditor debtor
    let hash = hashCreditRecord (lndrUcacAddr config) nonce signedRecord

    unless (T.length memo <= 32) $
        throwError (err400 {errBody = "Memo too long. Memos must be no longer than 32 characters."})
    unless (submitterAddress == creditor || submitterAddress == debtor) $
        throwError (err400 {errBody = "Submitter is not creditor nor debtor."})
    unless (creditor /= debtor) $
        throwError (err400 {errBody = "Creditor and debtor cannot be equal."})

    signer <- web3ToLndr . return . EU.ecrecover (stripHexPrefix sig) $ EU.hashPersonalMessage hash

    -- submitter signed the tx
    unless (textToAddress signer == submitterAddress) $
        throwError (err400 {errBody = "Bad submitter sig"})

    -- check if hash is already registered in pending txs
    pendingCredit <- liftIO . withResource pool $ Db.lookupPending hash

    let attemptToNotify msg notifyAction = do
            let counterparty = if creditor /= submitterAddress then debtor else creditor
            pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
            case pushDataM of
                -- TODO include nickname in the alert if we intend to use it
                Just (channelID, platform) -> void . liftIO $
                    sendNotification config (Notification channelID platform msg notifyAction)
                Nothing -> return ()

    case pendingCredit of
        -- if the submitted credit record has a matching pending record,
        -- finalize the transaction on the blockchain
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            let (creditorSig, debtorSig) = if creditor /= submitterAddress
                                            then (signature storedRecord, signature signedRecord)
                                            else (signature signedRecord, signature storedRecord)

            -- update gas price to latest safelow value
            updatedConfig <- safelowUpdate config configTVar

            finalizeTransaction updatedConfig creditorSig debtorSig storedRecord

            -- saving transaction record
            liftIO . withResource pool $ Db.insertCredit creditorSig debtorSig storedRecord
            -- delete pending record after transaction finalization
            void . liftIO . withResource pool $ Db.deletePending hash

            -- send push notification to counterparty
            attemptToNotify "New Pending Credit" NewPendingCredit

        -- if no matching transaction is found, create pending transaction
        Nothing -> do
            -- check if a pending transaction already exists between the two users
            existingPending <- liftIO . withResource pool $ Db.lookupPendingByAddresses creditor debtor
            unless (null existingPending) $
                throwError (err400 {errBody = "A pending credit record already exists for the two users."})

            -- ensuring that creditor is on debtor's friends list and vice-versa
            void . liftIO . withResource pool $ Db.addFriends creditor [debtor]
            void . liftIO . withResource pool $ Db.addFriends debtor [creditor]

            void . liftIO . withResource pool $ Db.insertPending (signedRecord { hash = hash })

            -- send push notification to counterparty
            attemptToNotify "Credit Confirmation" CreditConfirmation

    return NoContent


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.twoPartyNonce p1 p2
