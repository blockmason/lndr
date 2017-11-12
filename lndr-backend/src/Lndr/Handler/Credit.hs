module Lndr.Handler.Credit where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Text (Text)
import           ListT
import           Lndr.EthInterface
import           Lndr.Handler.Types
import           Lndr.Types
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import           Servant.API
import qualified STMContainers.Map as Map


-- submit a signed message consisting of "REJECT + CreditRecord HASH"
-- each credit record will be referenced by its hash
rejectHandler :: RejectRecord -> LndrHandler NoContent
rejectHandler(RejectRecord sig hash) = do
    -- TODO verify sig!
    pendingMapping <- pendingMap <$> ask
    liftIO . atomically $ Map.delete hash pendingMapping
    return NoContent


transactionsHandler :: Maybe Address -> LndrHandler [IssueCreditLog]
transactionsHandler _ = lndrWeb3 lndrLogs


pendingHandler :: Maybe Address -> LndrHandler [PendingRecord]
pendingHandler _ = do
    creditMap <- pendingMap <$> ask
    fmap (fmap snd) . liftIO . atomically . toList $ Map.stream creditMap


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
    pendingRecord <- liftIO . atomically $ Map.lookup hash creditMap
    let cr = creditRecord <$> pendingRecord

    case cr of
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
