{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Credit where

import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.List (nub)
import           Data.Pool (withResource)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           ListT
import qualified Lndr.Db as Db
import           Lndr.EthInterface
import           Lndr.Handler.Types
import           Lndr.Types
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant
import           Servant.API


rejectHandler :: RejectRecord -> LndrHandler NoContent
rejectHandler(RejectRecord sig hash) = do
    pool <- dbConnectionPool <$> ask
    pendingRecordM <- liftIO . withResource pool $ Db.lookupPending hash
    -- TODO this is easily de-cascaded
    case pendingRecordM of
        Nothing -> throwError $ err404 {errBody = "credit hash does not refer to pending record"}
        Just pr@(CreditRecord creditor debtor _ _ _ submitter _ _) -> do
            -- recover address from sig
            let signer = EU.ecrecover (stripHexPrefix sig) hash
            case signer of
                Left err -> throwError $ err400 {errBody = "unable to recover addr from sig"}
                Right addr -> if textToAddress addr == debtor || textToAddress addr == creditor
                                    then do liftIO . withResource pool $ Db.deletePending hash
                                            return NoContent
                                    else throwError $ err400 {errBody = "bad rejection sig"}


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler Nothing = do
    config <- serverConfig <$> ask
    lndrWeb3 (lndrLogs config Nothing Nothing)
transactionsHandler (Just addr) = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.lookupCreditByAddress addr


counterpartiesHandler :: Address -> LndrHandler [Address]
counterpartiesHandler addr = nub . fmap takeCounterParty <$> transactionsHandler (Just addr)
    where takeCounterParty (IssueCreditLog _ c d _ _ _) = if c == addr then d else c


balanceHandler :: Address -> LndrHandler Integer
balanceHandler addr = do
    config <- serverConfig <$> ask
    web3ToLndr $ queryBalance config addr


twoPartyBalanceHandler :: Address -> Address -> LndrHandler Integer
twoPartyBalanceHandler p1 p2 = do
    config <- serverConfig <$> ask
    debts <- sum . fmap extractAmount <$> lndrWeb3 (lndrLogs config (Just p2) (Just p1))
    credits <- sum . fmap extractAmount <$> lndrWeb3 (lndrLogs config (Just p1) (Just p2))
    return $ credits - debts
    where
        extractAmount (IssueCreditLog _ _ _ amount _ _) = amount


pendingHandler :: Address -> LndrHandler [CreditRecord]
pendingHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.lookupPendingByAddress addr


lendHandler :: CreditRecord -> LndrHandler NoContent
lendHandler creditRecord = submitHandler (creditor creditRecord) creditRecord


borrowHandler :: CreditRecord -> LndrHandler NoContent
borrowHandler creditRecord = submitHandler (debtor creditRecord) creditRecord


submitHandler :: Address -> CreditRecord -> LndrHandler NoContent
submitHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ memo _ _ _ sig) = do
    (ServerState pool config) <- ask
    (nonce, hash) <- lndrWeb3 $ hashCreditRecord config signedRecord

    if T.length memo <= 32
        then return ()
        else throwError (err400 {errBody = "Memo too long. Memos must be no longer than 32 characters."})

    -- verify that submitter is one of creditor or debtor
    if submitterAddress == creditor || submitterAddress == debtor
        then (if creditor /= debtor
                    then return ()
                    else throwError (err400 {errBody = "creditor and debtor cannot be equal"}))
        else throwError (err400 {errBody = "Submitter is not creditor nor debtor"})

    signer <- web3ToLndr . return . EU.ecrecover (stripHexPrefix sig) $ EU.hashPersonalMessage hash

    -- submitter signed the tx
    if textToAddress signer == submitterAddress
        then return ()
        else throwError (err400 {errBody = "Bad submitter sig"})

    -- check if hash is already registered in pending txs
    pendingCredit <- liftIO . withResource pool $ Db.lookupPending hash

    case pendingCredit of
        -- if the submitted credit record has a matching pending record,
        -- finalize the transaction on the blockchain
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            let (creditorSig, debtorSig) = if creditor /= submitterAddress
                                            then (signature storedRecord, signature signedRecord)
                                            else (signature signedRecord, signature storedRecord)

            finalizeTransaction config creditorSig debtorSig storedRecord

            -- saving transaction record
            liftIO . withResource pool $ Db.insertCredit creditorSig debtorSig storedRecord
            -- delete pending record after transaction finalization
            void . liftIO . withResource pool $ Db.deletePending hash

        -- if no matching transaction is found, create pending transaction
        Nothing -> void . liftIO . withResource pool $ Db.insertPending (signedRecord { hash = hash })

    return NoContent


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = do
    config <- serverConfig <$> ask
    fmap Nonce . web3ToLndr . runWeb3 $ queryNonce config p1 p2
