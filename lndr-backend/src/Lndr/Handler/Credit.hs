{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Credit (
    -- * credit submission handlers
      lendHandler
    , borrowHandler
    , rejectHandler
    , verifyHandler

    -- * app state-querying handlers
    , pendingHandler
    , pendingSettlementsHandler
    , txHashHandler
    , transactionsHandler
    , nonceHandler
    , counterpartiesHandler
    , balanceHandler
    , twoPartyBalanceHandler
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Maybe (fromMaybe)
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple (Connection)
import qualified Lndr.Db as Db
import           Lndr.EthereumInterface
import           Lndr.Handler.Types
import           Lndr.Notifications
import           Lndr.NetworkStatistics
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant


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
    signer <- ioEitherToLndr . return . EU.ecrecover (stripHexPrefix sig) $ EU.hashPersonalMessage hash
    unless (textToAddress signer == submitterAddress) $
        throwError (err401 {errBody = "Bad submitter sig"})

-- TODO fix this
submitHandler :: Address -> CreditRecord -> LndrHandler NoContent
submitHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ memo _ _ _ sig _ _ _ _) = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    nonce <- liftIO . withResource pool $ Db.twoPartyNonce creditor debtor
    settlementM <- liftIO . runMaybeT $ settlementDataFromCreditRecord signedRecord

    let hash = hashCreditRecord nonce signedRecord

    -- check that credit submission is valid
    validSubmission memo submitterAddress creditor debtor sig hash

    -- creating function to query urban airship api
    let attemptToNotify msg notifyAction = do
            let counterparty = if creditor /= submitterAddress then creditor else debtor
            pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
            nicknameM <- liftIO . withResource pool $ Db.lookupNick submitterAddress
            let fullMsg = T.append msg (fromMaybe "..." nicknameM)
            case pushDataM of
                Just (channelID, platform) -> void . liftIO $
                    sendNotification config (Notification channelID platform fullMsg notifyAction)
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

            finalizeCredit pool storedRecord updatedConfig creditorSig debtorSig hash settlementM

            -- send push notification to counterparty
            attemptToNotify "Pending credit confirmation from " CreditConfirmation

        -- if no matching transaction is found, create pending transaction
        Nothing -> do
            createPendingRecord pool creditor debtor signedRecord hash settlementM
            -- send push notification to counterparty
            attemptToNotify "New pending credit from " NewPendingCredit

    return NoContent


finalizeCredit :: Pool Connection -> CreditRecord -> ServerConfig -> Text -> Text -> Text -> Maybe SettlementData -> IO ()
finalizeCredit pool storedRecord config creditorSig debtorSig hash settlementM = do
            -- In case the record is a settlement, delay submitting credit to
            -- the blockchain until /verify_settlement is called
            case settlementM of
                Just _  -> return ()
                Nothing -> void $ finalizeTransaction config creditorSig debtorSig storedRecord

            -- saving transaction record
            withResource pool $ Db.insertCredit creditorSig debtorSig storedRecord
            -- delete pending record after transaction finalization
            void . withResource pool $ Db.deletePending hash False


createPendingRecord :: Pool Connection -> Address -> Address -> CreditRecord -> Text -> Maybe SettlementData -> LndrHandler ()
createPendingRecord pool creditor debtor signedRecord hash settlementM = do
    -- check if a pending transaction already exists between the two users
    existingPending <- liftIO . withResource pool $ Db.lookupPendingByAddresses creditor debtor
    unless (null existingPending) $
        throwError (err400 {errBody = "A pending credit record already exists for the two users."})

    -- check if an unverified bilateral credit record exists in the
    -- `verified_credits` table
    existingPendingSettlement <- liftIO . withResource pool $ Db.lookupPendingSettlementByAddresses creditor debtor
    unless (null existingPendingSettlement) $
        throwError (err400 {errBody = "An unverified settlement credit record already exists for the two users."})

    -- ensuring that creditor is on debtor's friends list and vice-versa
    liftIO $ createBilateralFriendship pool creditor debtor

    void . liftIO . withResource pool $ Db.insertPending (signedRecord { hash = hash }) settlementM


createBilateralFriendship :: Pool Connection -> Address -> Address -> IO ()
createBilateralFriendship pool creditor debtor = do
            withResource pool $ Db.addFriends creditor [debtor]
            void . withResource pool $ Db.addFriends debtor [creditor]


rejectHandler :: RejectRequest -> LndrHandler NoContent
rejectHandler(RejectRequest hash sig) = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    pendingRecordM <- liftIO . withResource pool $ Db.lookupPending hash
    let hashNotFound = throwError $ err404 { errBody = "credit hash does not refer to pending record" }
    -- TODO clean this up
    (CreditRecord creditor debtor _ _ _ _ _ _ _ _ _ _) <- maybe hashNotFound pure pendingRecordM
    -- recover address from sig
    let signer = EU.ecrecover (stripHexPrefix sig) hash
    case signer of
        Left _ -> throwError $ err401 { errBody = "unable to recover addr from sig" }
        Right addr -> if textToAddress addr == debtor || textToAddress addr == creditor
            then do liftIO . withResource pool $ Db.deletePending hash True
                    let submitterAddress = textToAddress addr
                        counterparty = if creditor /= submitterAddress then creditor else debtor
                    pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
                    nicknameM <- liftIO . withResource pool $ Db.lookupNick submitterAddress
                    let fullMsg = T.append "Pending credit rejected by " (fromMaybe "..." nicknameM)
                    case pushDataM of
                        Just (channelID, platform) -> void . liftIO $
                            sendNotification config (Notification channelID platform fullMsg PendingCreditRejection)
                        Nothing -> return ()

                    return NoContent
            else throwError $ err401 { errBody = "bad rejection sig" }


verifyHandler :: VerifySettlementRequest -> LndrHandler NoContent
verifyHandler r@(VerifySettlementRequest creditHash txHash creditorAddress signature) = do
    unless (Right creditorAddress == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})

    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar

    -- write txHash to settlement record
    liftIO . withResource pool . Db.updateSettlementTxHash creditHash $ stripHexPrefix txHash
    return NoContent


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
    liftIO $ withResource pool $ Db.lookupCreditByAddress addr


pendingSettlementsHandler :: Address -> LndrHandler SettlementsResponse
pendingSettlementsHandler addr = do
    pool <- dbConnectionPool <$> ask
    pending <- liftIO . withResource pool $ Db.lookupPendingByAddress addr True
    verified <- liftIO $ withResource pool $ Db.lookupSettlementCreditByAddress addr
    return $ SettlementsResponse pending verified


txHashHandler :: Text -> LndrHandler Text
txHashHandler creditHash = do
    pool <- dbConnectionPool <$> ask
    txHashM <- liftIO . withResource pool $ Db.txHashByCreditHash creditHash
    case txHashM of
        (Just txHash) -> return txHash
        Nothing -> throwError $ err404 { errBody = "Settlement credit not found" }


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
