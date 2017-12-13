module Lndr.Handler.Admin where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Lndr.Handler.Types
import           Lndr.Types
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
