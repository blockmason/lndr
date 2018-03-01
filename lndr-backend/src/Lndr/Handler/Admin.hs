{-# LANGUAGE OverloadedStrings   #-}

module Lndr.Handler.Admin where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.List ((\\), find)
import           Data.Pool (withResource)
import           Data.Text (Text)
import qualified Lndr.Db as Db
import           Lndr.Handler.Types
import           Lndr.EthereumInterface
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import           Lndr.Web3
import           Network.Ethereum.Web3
import           Servant


gasPriceHandler :: LndrHandler Integer
gasPriceHandler = do
    configTVar <- serverConfig <$> ask
    config <- liftIO . atomically $ readTVar configTVar
    pure $ gasPrice config


setGasPriceHandler :: Integer -> LndrHandler NoContent
setGasPriceHandler newGasPrice = do
    configTVar <- serverConfig <$> ask
    liftIO . atomically $ modifyTVar configTVar (\x -> x { gasPrice = newGasPrice })
    pure NoContent


unsubmittedHandler :: LndrHandler (Int, Int, [IssueCreditLog])
unsubmittedHandler = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    blockchainCreditsE <- liftIO . runLndrWeb3 $
        join <$> sequence [ lndrLogs config "USD" Nothing Nothing
                          , lndrLogs config "JPY" Nothing Nothing
                          , lndrLogs config "KRW" Nothing Nothing ]

    let blockchainCredits = either (const []) id blockchainCreditsE
    dbCredits <- liftIO $ withResource pool Db.allCredits
    pure (length dbCredits, length blockchainCredits, dbCredits \\ blockchainCredits)


resubmitHandler :: Text -> LndrHandler NoContent
resubmitHandler txHash = do
    (ServerState pool configTVar) <- ask
    (_, _, txs) <- unsubmittedHandler
    let creditToResubmitM = find ((== txHash) . hashCreditLog) txs
    case creditToResubmitM of
        Just creditLog -> do
            config <- liftIO . atomically $ readTVar configTVar
            let creditHash = hashCreditLog creditLog
            bcrM <- liftIO . withResource pool $ Db.lookupCreditByHash creditHash
            case bcrM of
                Just bilateralCreditRecord -> void . liftIO $ finalizeTransaction config bilateralCreditRecord
                Nothing               -> pure ()
        Nothing -> pure ()
    pure NoContent


registerPushHandler :: PushRequest -> LndrHandler NoContent
registerPushHandler r@(PushRequest channelID platform addr _) = do
    unless (Right addr == recoverSigner r) $ throwError (err400 {errBody = "Bad signature."})
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.insertPushDatum addr channelID platform
    pure NoContent


configHandler :: LndrHandler ConfigResponse
configHandler = do
    configTVar <- serverConfig <$> ask
    configToResponse <$> (liftIO . atomically $ readTVar configTVar)
