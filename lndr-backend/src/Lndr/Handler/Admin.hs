module Lndr.Handler.Admin where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Lndr.Handler.Types
import           Lndr.Types

gasPriceHandler :: LndrHandler Integer
gasPriceHandler = do
    configMVar <- serverConfig <$> ask
    config <- liftIO $ takeMVar configMVar
    return $ gasPrice config
