module Lndr.Handler.Credit where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           ListT
import           Lndr.EthInterface
import           Lndr.Handler.Types
import           Lndr.Types
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant.API
import qualified STMContainers.Map as Map


-- submit a signed message consisting of "REJECT + CreditRecord HASH"
rejectHandler :: RejectRecord -> LndrHandler NoContent
rejectHandler(RejectRecord sig hash) = do
    pendingMapping <- pendingMap <$> ask
    pendingRecordM <- liftIO . atomically $ Map.lookup hash pendingMapping
    case pendingRecordM of
        Nothing -> throwError $ LndrError "credit hash does not refer to pending record"
        Just pr@(PendingRecord (CreditRecord c d _ _ _) s _) -> do
            liftIO . atomically $ Map.delete hash pendingMapping
            let counterparty = if textToAddress c == s then d else c
            -- recover address from sig
            let message = hashPrefixedMessage "REJECT" hash
            let signer = EU.ecrecover sig $ EU.hashPersonalMessage hash
            case signer of
                Left err -> throwError $ LndrError err
                Right addr -> if textToAddress addr == textToAddress counterparty
                                    then return ()
                                    else throwError $ LndrError "bad rejection sig"

            -- verify that address is counterparty
            liftIO . atomically $ Map.delete hash pendingMapping
            return NoContent


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler _ = lndrWeb3 lndrLogs


pendingHandler :: Maybe Address -> LndrHandler [PendingRecord]
pendingHandler addrM = do
    creditMap <- pendingMap <$> ask
    fmap processRecords . liftIO . atomically . toList $ Map.stream creditMap

    where involves (Just addr) x = addr == textToAddress (creditor $ creditRecord x)
                                || addr == textToAddress (debtor $ creditRecord x)
          involves Nothing _ = True
          processRecords = filter (involves addrM) . fmap snd

lendHandler :: CreditRecord Signed -> LndrHandler NoContent
lendHandler cr@(CreditRecord creditor _ _ _ _) = submitSignedHandler creditor cr


borrowHandler :: CreditRecord Signed -> LndrHandler NoContent
borrowHandler cr@(CreditRecord _ debtor _ _ _) = submitSignedHandler debtor cr


submitSignedHandler :: Text -> CreditRecord Signed
                    -> LndrHandler NoContent
submitSignedHandler submitterAddress signedRecord@(CreditRecord creditor debtor _ _ sig) = do
    creditMap <- pendingMap <$> ask
    (nonce, hash) <- lndrWeb3 $ hashCreditRecord signedRecord

    -- submitter is one of creditor or debtor
    if submitterAddress == creditor || submitterAddress == debtor
        then return ()
        else throwError (LndrError "Submitter is not creditor nor debtor")

    signer <- web3ToLndr . return $ EU.ecrecover sig $ EU.hashPersonalMessage hash

    -- submitter signed the tx
    if signer == submitterAddress
        then return ()
        else throwError (LndrError "Bad submitter sig")

    -- check if hash is already registered in pending txs
    pendingCredit <- fmap (fmap creditRecord) . liftIO . atomically $
                                                    Map.lookup hash creditMap

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
            atomically $ Map.delete hash creditMap

        -- if no matching transaction is found, create pending transaction
        Nothing -> liftIO . atomically $
                        Map.insert (PendingRecord signedRecord submitterAddr hash)
                                   hash creditMap

    return NoContent
    where submitterAddr = textToAddress submitterAddress


nonceHandler :: Address -> Address -> LndrHandler Nonce
nonceHandler p1 p2 = fmap Nonce . web3ToLndr . runWeb3 $ queryNonce p1 p2
