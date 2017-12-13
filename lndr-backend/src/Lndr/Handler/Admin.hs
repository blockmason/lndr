module Lndr.Handler.Admin where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.List ((\\))
import           Data.Pool (withResource)
import qualified Lndr.Db as Db
import           Lndr.Handler.Types
import           Lndr.EthInterface
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant.API


gasPriceHandler :: LndrHandler Integer
gasPriceHandler = do
    configTVar <- serverConfig <$> ask
    config <- liftIO . atomically $ readTVar configTVar
    return $ gasPrice config


setGasPriceHandler :: Integer -> LndrHandler NoContent
setGasPriceHandler newGasPrice = do
    configTVar <- serverConfig <$> ask
    liftIO . atomically $ modifyTVar configTVar (\x -> x { gasPrice = newGasPrice })
    return NoContent

unsubmittedHandler :: LndrHandler [IssueCreditLog]
unsubmittedHandler = do
    (ServerState pool configTVar) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    blockchainCreditsE <- liftIO . runWeb3 $ lndrLogs config Nothing Nothing
    let blockchainCredits = either (const []) id blockchainCreditsE
    dbCredits <- liftIO $ withResource pool Db.allCredits
    return $ (setUcac (lndrUcacAddr config) <$> dbCredits) \\ blockchainCredits
