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
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.Bimap                 as B
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, fromJust, isNothing)
import           Data.Pool                  (Pool, withResource)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, begin, commit)
import qualified Lndr.Db                    as Db
import           Lndr.EthereumInterface
import           Lndr.Handler.Types
import           Lndr.Notifications
import           Lndr.NetworkStatistics
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import qualified Network.Ethereum.Util      as EU
import           Network.Ethereum.Web3
import           Servant
import           System.Log.FastLogger
import           Text.Printf


lendHandler :: CreditRecord -> LndrHandler NoContent
lendHandler creditRecord = submitHandler $ creditRecord { submitter = creditor creditRecord }


borrowHandler :: CreditRecord -> LndrHandler NoContent
borrowHandler creditRecord = submitHandler $ creditRecord { submitter = debtor creditRecord }


submitHandler :: CreditRecord -> LndrHandler NoContent
submitHandler signedRecord@(CreditRecord creditor debtor _ memo submitterAddress _ hash sig _ _ _ _) = do
    (ServerState pool configTVar loggerSet) <- ask
    config <- liftIO $ readTVarIO configTVar
    nonce <- liftIO . withResource pool $ Db.twoPartyNonce creditor debtor

    -- check that credit submission is valid
    unless (hash == generateHash signedRecord) $
        throwError (err400 {errBody = "Bad hash included with credit record."})
    unless (T.length memo <= 32) $
        throwError (err400 {errBody = "Memo too long. Memos must be no longer than 32 characters."})
    unless (submitterAddress == creditor || submitterAddress == debtor) $
        throwError (err400 {errBody = "Submitter is not creditor nor debtor."})
    unless (creditor /= debtor) $
        throwError (err400 {errBody = "Creditor and debtor cannot be equal."})
    unless (B.memberR (ucac signedRecord) (lndrUcacAddrs config)) $
        throwError (err400 {errBody = "Unrecognized UCAC address."})

    -- check that submitter signed the tx
    signer <- ioEitherToLndr . pure . EU.ecrecover (stripHexPrefix sig) $
                EU.hashPersonalMessage hash
    unless (textToAddress signer == submitterAddress) $
        throwError (err401 {errBody = "Bad submitter sig"})

    currency <- liftIO $ B.lookupR (ucac signedRecord) (lndrUcacAddrs config)

    -- creating function to query urban airship api
    let attemptToNotify msg notifyAction = do
            let counterparty = if creditor /= submitterAddress then creditor else debtor
            pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
            nicknameM <- liftIO . withResource pool $ Db.lookupNick submitterAddress
            let fullMsg = T.pack $ printf msg (fromMaybe "..." nicknameM)

            forM_ pushDataM $ \(channelID, platform) -> liftIO $ do
                repsonseCode <- sendNotification config (Notification channelID platform fullMsg notifyAction)
                let logMsg = "UrbanAirship response (" ++ T.unpack currency ++ "): " ++ show repsonseCode
                pushLogStrLn loggerSet . toLogStr $ logMsg

    -- check if hash is already registered in pending txs
    pendingCredit <- liftIO . withResource pool $ Db.lookupPending hash
    case pendingCredit of
        -- if the submitted credit record has a matching pending record,
        -- finalize the transaction on the blockchain
        Just storedRecord -> when (signature storedRecord /= signature signedRecord) $ do
            let (creditorSig, debtorSig) = if creditor /= submitterAddress
                                            then (signature storedRecord, signature signedRecord)
                                            else (signature signedRecord, signature storedRecord)

            finalizeCredit $ BilateralCreditRecord storedRecord creditorSig debtorSig Nothing

            -- send push notification to counterparty
            attemptToNotify (fromJust $ M.lookup (T.unpack currency) pendingConfirmationMap) CreditConfirmation

        -- if no matching transaction is found, create pending transaction
        Nothing -> do
            let processedRecord = calculateSettlementCreditRecord config signedRecord
            createPendingRecord pool processedRecord
            -- send push notification to counterparty
            attemptToNotify (fromJust $ M.lookup (T.unpack currency) newPendingMap) NewPendingCredit

    pure NoContent


finalizeCredit :: BilateralCreditRecord -> LndrHandler ()
finalizeCredit bilateralCredit = do
            (ServerState pool configTVar loggerSet) <- ask
            -- In case the record is a settlement, delay submitting credit to
            -- the blockchain until /verify_settlement is called
            -- (settlements will have 'Just _' for their 'settlementAmount',
            -- non-settlements will have 'Nothing')
            when (isNothing . settlementAmount . creditRecord $ bilateralCredit) $ do
                web3Result <- finalizeTransaction configTVar bilateralCredit
                                    `catchError` (pure . T.pack . show)
                liftIO $ pushLogStrLn loggerSet . toLogStr . ("WEB3: " ++) . show $ web3Result

            -- saving transaction record to 'verified_credits' table
            liftIO . withResource pool $ \conn -> do
                begin conn
                Db.insertCredit bilateralCredit conn
                -- delete pending record after transaction finalization
                Db.deletePending (hash $ creditRecord bilateralCredit) False conn
                commit conn


createPendingRecord :: Pool Connection -> CreditRecord -> LndrHandler ()
createPendingRecord pool signedRecord = do
    -- TODO can I consolidate these two checks into one?
    -- check if a pending transaction already exists between the two users
    existingPending <- liftIO . withResource pool $ Db.lookupPendingByAddresses (creditor signedRecord) (debtor signedRecord)
    unless (null existingPending) $
        throwError (err400 {errBody = "A pending credit record already exists for the two users."})

    -- check if an unverified bilateral credit record exists in the
    -- `verified_credits` table
    existingPendingSettlement <- liftIO . withResource pool $
        Db.lookupPendingSettlementByAddresses (creditor signedRecord) (debtor signedRecord)
    unless (null existingPendingSettlement) $
        throwError (err400 {errBody = "An unverified settlement credit record already exists for the two users."})

    -- ensuring that creditor is on debtor's friends list and vice-versa
    liftIO $ createBilateralFriendship pool (creditor signedRecord) (debtor signedRecord)

    void . liftIO . withResource pool $ Db.insertPending signedRecord


calculateSettlementCreditRecord :: ServerConfig -> CreditRecord -> CreditRecord
calculateSettlementCreditRecord _ cr@(CreditRecord _ _ _ _ _ _ _ _ _ _ Nothing _) = cr
calculateSettlementCreditRecord config cr@(CreditRecord _ _ amount _ _ _ _ _ ucac _ (Just currency) _) =
    let blockNumber = latestBlockNumber config
        prices = ethereumPrices config
        priceAdjustmentForCents = 100
        currencyPerEth = case B.lookupR ucac (lndrUcacAddrs config) of
            Just "USD" -> usd prices * priceAdjustmentForCents
            Just "JPY" -> jpy prices
            Just "KRW" -> krw prices
            Just "DKK" -> dkk prices * priceAdjustmentForCents
            Just "CHF" -> chf prices * priceAdjustmentForCents
            Just "CNY" -> cny prices * priceAdjustmentForCents
            Just "EUR" -> eur prices * priceAdjustmentForCents
            Just "AUD" -> aud prices * priceAdjustmentForCents
            Just "GBP" -> gbp prices * priceAdjustmentForCents
            Just "CAD" -> cad prices * priceAdjustmentForCents
            Just "NOK" -> nok prices * priceAdjustmentForCents
            Just "SEK" -> sek prices * priceAdjustmentForCents
            Just "NZD" -> nzd prices * priceAdjustmentForCents
            Nothing    -> error "ucac not found"
        settlementAmountRaw = floor $ fromIntegral amount / currencyPerEth * 10 ^ 18
    in cr { settlementAmount = Just $ roundToMegaWei settlementAmountRaw
          , settlementBlocknumber = Just blockNumber
          }


createBilateralFriendship :: Pool Connection -> Address -> Address -> IO ()
createBilateralFriendship pool addressA addressB =
    void . withResource pool $ Db.addFriends [(addressA, addressB), (addressB, addressA)]


rejectHandler :: RejectRequest -> LndrHandler NoContent
rejectHandler(RejectRequest hash sig) = do
    pool <- asks dbConnectionPool
    pendingRecord <- ioMaybeToLndr "credit hash does not refer to pending record"
                        . withResource pool $ Db.lookupPending hash
    signerAddress <- eitherToLndr "unable to recover addr from sig" $
                        textToAddress <$> EU.ecrecover (stripHexPrefix sig) hash
    unless (signerAddress == debtor pendingRecord || signerAddress == creditor pendingRecord) $
        throwError $ err401 { errBody = "bad rejection sig" }

    liftIO . withResource pool $ Db.deletePending hash True
    sendRejectionNotification pendingRecord signerAddress
    pure NoContent


sendRejectionNotification :: CreditRecord -> Address -> LndrHandler ()
sendRejectionNotification pendingRecord signerAddress = do
    (ServerState pool configTVar loggerSet) <- ask
    config <- liftIO $ readTVarIO configTVar
    currency <- liftIO $ B.lookupR (ucac pendingRecord) (lndrUcacAddrs config)
    let counterparty = if creditor pendingRecord /= signerAddress
                            then creditor pendingRecord
                            else debtor pendingRecord
    pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress counterparty
    nicknameM <- liftIO . withResource pool $ Db.lookupNick signerAddress
    let msgTemplate = fromJust $ M.lookup (T.unpack currency) pendingRejectionMap
        fullMsg = T.pack $ printf msgTemplate (fromMaybe "..." nicknameM)
    forM_ pushDataM $ \(channelID, platform) -> liftIO $ do
            responseCode <- sendNotification config (Notification channelID platform fullMsg PendingCreditRejection)
            let logMsg =  "UrbanAirship response (" ++ T.unpack currency ++ "): " ++ show responseCode
            pushLogStrLn loggerSet . toLogStr $ logMsg


verifyHandler :: VerifySettlementRequest -> LndrHandler NoContent
verifyHandler r@(VerifySettlementRequest creditHash txHash creditorAddress signature) = do
    unless (Right creditorAddress == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})

    (ServerState pool configTVar _) <- ask
    config <- liftIO $ readTVarIO configTVar

    -- write txHash to settlement record
    liftIO . withResource pool . Db.updateSettlementTxHash creditHash $ stripHexPrefix txHash
    pure NoContent


pendingHandler :: Address -> LndrHandler [CreditRecord]
pendingHandler addr = do
    pool <- asks dbConnectionPool
    liftIO . withResource pool $ Db.lookupPendingByAddress addr False


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler Nothing = throwError (err401 {errBody = "Missing user address"})
transactionsHandler (Just addr) = do
    pool <- asks dbConnectionPool
    liftIO $ withResource pool $ Db.lookupCreditByAddress addr


pendingSettlementsHandler :: Address -> LndrHandler SettlementsResponse
pendingSettlementsHandler addr = do
    pool <- asks dbConnectionPool
    pending <- liftIO . withResource pool $ Db.lookupPendingByAddress addr True
    verified <- liftIO $ withResource pool $ Db.lookupSettlementCreditByAddress addr
    pure $ SettlementsResponse pending verified


txHashHandler :: Text -> LndrHandler Text
txHashHandler creditHash = do
    pool <- asks dbConnectionPool
    txHashM <- liftIO . withResource pool $ Db.txHashByCreditHash creditHash
    case txHashM of
        (Just txHash) -> pure txHash
        Nothing -> throwError $ err404 { errBody = "Settlement credit not found" }


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = do
    pool <- asks dbConnectionPool
    liftIO . withResource pool $ Db.twoPartyNonce p1 p2


counterpartiesHandler :: Address -> LndrHandler [Address]
counterpartiesHandler addr = do
    pool <- asks dbConnectionPool
    liftIO $ withResource pool $ Db.counterpartiesByAddress addr


balanceHandler :: Address -> Maybe Text -> LndrHandler Integer
balanceHandler addr currency = do
    (ServerState pool configTVar _) <- ask
    ucacAddresses <- fmap lndrUcacAddrs . liftIO $ readTVarIO configTVar
    liftIO . withResource pool $ Db.userBalance addr (getUcac ucacAddresses currency)


twoPartyBalanceHandler :: Address -> Address -> Maybe Text -> LndrHandler Integer
twoPartyBalanceHandler p1 p2 currency = do
    (ServerState pool configTVar _) <- ask
    ucacAddresses <- fmap lndrUcacAddrs . liftIO $ readTVarIO configTVar
    liftIO . withResource pool $ Db.twoPartyBalance p1 p2 (getUcac ucacAddresses currency)
