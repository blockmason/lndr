module Lndr.Handler.Admin where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Lndr.Handler.Types
import           Lndr.Types
import           Servant.API


gasPriceHandler :: LndrHandler Integer
gasPriceHandler = do
    configMVar <- serverConfig <$> ask
    config <- liftIO $ takeMVar configMVar
    return $ gasPrice config


setGasPriceHandler :: Integer -> LndrHandler NoContent
setGasPriceHandler newGasPrice = do
    configMVar <- serverConfig <$> ask
    config <- liftIO $ takeMVar configMVar
    liftIO . putMVar configMVar $ config { gasPrice = newGasPrice }
    return NoContent
