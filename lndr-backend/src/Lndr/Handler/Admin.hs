module Lndr.Handler.Admin where

import           Control.Monad.Reader
import           Lndr.Handler.Types
import           Lndr.Types

gasPriceHandler :: LndrHandler Integer
gasPriceHandler = do
    config <- serverConfig <$> ask
    return $ gasPrice config
