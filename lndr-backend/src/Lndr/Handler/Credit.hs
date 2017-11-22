{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Credit where

import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.List (nub)
import           Data.Pool (withResource)
import           Data.Text (Text)
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
            let signer = EU.ecrecover (stripHexPrefix sig) $ hash
            case signer of
                Left err -> throwError $ err400 {errBody = "unable to recover addr from sig"}
                Right addr -> if textToAddress addr == debtor || textToAddress addr == creditor
                                    then do liftIO . withResource pool $ Db.deletePending hash
                                            return NoContent
                                    else throwError $ err400 {errBody = "bad rejection sig"}


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler Nothing = lndrWeb3 (lndrLogs Nothing Nothing)
transactionsHandler (Just addr) = (++) <$> lndrWeb3 (lndrLogs (Just addr) Nothing)
                                       <*> lndrWeb3 (lndrLogs Nothing (Just addr))


counterpartiesHandler :: Address -> LndrHandler [Address]
counterpartiesHandler addr = nub . fmap takeCounterParty <$> transactionsHandler (Just addr)
    where takeCounterParty (IssueCreditLog _ c d _ _) = if c == addr then d else c


balanceHandler :: Address -> LndrHandler Integer
balanceHandler addr = web3ToLndr $ queryBalance addr


twoPartyBalanceHandler :: Address -> Address -> LndrHandler Integer
twoPartyBalanceHandler p1 p2 = do
    debts <- sum . fmap extractAmount <$> lndrWeb3 (lndrLogs (Just p2) (Just p1))
    credits <- sum . fmap extractAmount <$> lndrWeb3 (lndrLogs (Just p1) (Just p2))
    return $ credits - debts
    where
        -- TODO lens needed
        extractAmount (IssueCreditLog _ _ _ amount _) = amount


pendingHandler :: Address -> LndrHandler [CreditRecord]
pendingHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO $ withResource pool $ Db.lookupPendingByAddress addr


lendHandler :: CreditRecord -> LndrHandler NoContent
lendHandler cr@(CreditRecord creditor _ _ _ _ _ _ _) = submitHandler creditor cr


borrowHandler :: CreditRecord -> LndrHandler NoContent
borrowHandler cr@(CreditRecord _ debtor _ _ _ _ _ _) = submitHandler debtor cr


submitHandler :: Address -> CreditRecord -> LndrHandler NoContent
submitHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ _ _ _ _ sig) = do
    pool <- dbConnectionPool <$> ask
    (nonce, hash) <- lndrWeb3 $ hashCreditRecord signedRecord

    -- TODO verify that credit record memo is under 32 chars (all validation
    -- should happen in separate function perhaps

    -- TODO hash matches

    -- submitter is one of creditor or debtor
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
        Just storedRecord -> liftIO . when (signature storedRecord /= signature signedRecord) $ do
            -- if the submitted credit record has a matching pending record,
            -- finalize the transaction on the blockchain
            if creditor /= submitterAddress
                then finalizeTransaction (signature storedRecord)
                                         (signature signedRecord)
                                         storedRecord
                else finalizeTransaction (signature signedRecord)
                                         (signature storedRecord)
                                         storedRecord
            -- delete pending record after transaction finalization
            void . liftIO . withResource pool $ Db.deletePending hash

        -- if no matching transaction is found, create pending transaction
        Nothing -> void . liftIO . withResource pool $ Db.insertPending (signedRecord { hash = hash })

    return NoContent


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = fmap Nonce . web3ToLndr . runWeb3 $ queryNonce p1 p2
