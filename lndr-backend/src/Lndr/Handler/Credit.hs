{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Credit (
    -- * credit submission handlers
      settleHandler
    , lendHandler
    , borrowHandler
    , rejectHandler
    , verifyHandler

    -- * app state-querying handlers
    , pendingHandler
    , pendingSettlementsHandler
    , transactionsHandler
    , nonceHandler
    , counterpartiesHandler
    , balanceHandler
    , twoPartyBalanceHandler
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple (Connection)
import qualified Lndr.Db as Db
import           Lndr.EthereumInterface
import           Lndr.Handler.Types
import           Lndr.Notifications
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Util
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant


settleHandler :: CreditRecord -> LndrHandler NoContent
settleHandler creditRecord = submitHandler (creditor creditRecord) creditRecord


lendHandler :: CreditRecord -> LndrHandler NoContent
lendHandler creditRecord = submitHandler (creditor creditRecord) creditRecord


borrowHandler :: CreditRecord -> LndrHandler NoContent
borrowHandler creditRecord = submitHandler (debtor creditRecord) creditRecord


validSubmission :: Text -> Address -> Address -> Address -> Text -> Text -> LndrHandler ()
validSubmission memo submitterAddress creditor debtor sig hash = do
    unless (T.length memo <= 32) $
        throwError (err400 {errBody = "Memo too long. Memos must be no longer than 32 characters."})
    unless (submitterAddress == creditor || submitterAddress == debtor) $
        throwError (err400 {errBody = "Submitter is not creditor nor debtor."})
    unless (creditor /= debtor) $
        throwError (err400 {errBody = "Creditor and debtor cannot be equal."})

    -- check that submitter signed the tx
    signer <- web3ToLndr . return . EU.ecrecover (stripHexPrefix sig) $ EU.hashPersonalMessage hash
    unless (textToAddress signer == submitterAddress) $
        throwError (err400 {errBody = "Bad submitter sig"})


submitHandler :: Address -> CreditRecord -> LndrHandler NoContent
submitHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ memo _ _ _ sig) = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    nonce <- liftIO . withResource pool $ Db.twoPartyNonce creditor debtor
    let hash = hashCreditRecord (lndrUcacAddr config) nonce signedRecord

    -- check that credit submission is valid
    validSubmission memo submitterAddress creditor debtor sig hash

    -- creating function to query urban airship api
    let attemptToNotify msg notifyAction = do
            let counterparty = if creditor /= submitterAddress then debtor else creditor
            pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
            case pushDataM of
                -- TODO include nickname in the alert if we intend to use it
                Just (channelID, platform) -> void . liftIO $
                    sendNotification config (Notification channelID platform msg notifyAction)
                Nothing -> return ()

    -- check if hash is already registered in pending txs
    pendingCredit <- liftIO . withResource pool $ Db.lookupPending hash
    case pendingCredit of
        -- if the submitted credit record has a matching pending record,
        -- finalize the transaction on the blockchain
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            let (creditorSig, debtorSig) = if creditor /= submitterAddress
                                            then (signature storedRecord, signature signedRecord)
                                            else (signature signedRecord, signature storedRecord)

            -- update gas price to latest safelow value
            updatedConfig <- safelowUpdate config configTVar

            finalizeCredit pool storedRecord updatedConfig creditorSig debtorSig hash

            -- send push notification to counterparty
            attemptToNotify "Credit Confirmation" CreditConfirmation

        -- if no matching transaction is found, create pending transaction
        Nothing -> do
            createPendingRecord pool creditor debtor signedRecord hash
            -- send push notification to counterparty
            attemptToNotify "New Pending Credit" NewPendingCredit

    return NoContent


finalizeCredit :: Pool Connection -> CreditRecord -> ServerConfig -> Text -> Text -> Text -> IO ()
finalizeCredit pool storedRecord config creditorSig debtorSig hash = do
            finalizeTransaction config creditorSig debtorSig storedRecord
            -- saving transaction record
            withResource pool $ Db.insertCredit creditorSig debtorSig storedRecord
            -- delete pending record after transaction finalization
            void . withResource pool $ Db.deletePending hash


createPendingRecord :: Pool Connection -> Address -> Address -> CreditRecord -> Text -> LndrHandler ()
createPendingRecord pool creditor debtor signedRecord hash = do
    -- check if a pending transaction already exists between the two users
    existingPending <- liftIO . withResource pool $ Db.lookupPendingByAddresses creditor debtor
    unless (null existingPending) $
        throwError (err400 {errBody = "A pending credit record already exists for the two users."})

    -- ensuring that creditor is on debtor's friends list and vice-versa
    liftIO $ createBilateralFriendship pool creditor debtor

    void . liftIO . withResource pool $ Db.insertPending (signedRecord { hash = hash })


createBilateralFriendship :: Pool Connection -> Address -> Address -> IO ()
createBilateralFriendship pool creditor debtor = do
            withResource pool $ Db.addFriends creditor [debtor]
            void . withResource pool $ Db.addFriends debtor [creditor]


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


-- TODO for now we'll assume Just txHash, eventually, the server will be smart
-- enough to look for the tx automatically
verifyHandler :: Address -> Address -> Maybe Text -> LndrHandler NoContent
verifyHandler debtor creditor (Just txHash) = do
    -- TODO replace the 0 here with the value that we grab from the db
    verified <- liftIO $ verifySettlementPayment txHash debtor creditor 0
    if verified
        then return NoContent -- flip the settlement bit off
        else throwError $ err400 { errBody = "Unable to verify debt settlement" }

pendingHandler :: Address -> LndrHandler [CreditRecord]
pendingHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.lookupPendingByAddress addr False


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler Nothing = do
    configTVar <- serverConfig <$> ask
    config <- liftIO . atomically $ readTVar configTVar
    lndrWeb3 (lndrLogs config Nothing Nothing)
transactionsHandler (Just addr) = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.lookupCreditByAddress addr False


pendingSettlementsHandler :: Address -> LndrHandler ([CreditRecord], [IssueCreditLog])
pendingSettlementsHandler addr = do
    pool <- dbConnectionPool <$> ask
    pending <- liftIO . withResource pool $ Db.lookupPendingByAddress addr True
    verified <- liftIO $ withResource pool $ Db.lookupCreditByAddress addr True
    return (pending, verified)


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.twoPartyNonce p1 p2


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
