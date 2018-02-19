module Lndr.Web3 where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Lndr.Config
import           Lndr.Types
import           Network.Ethereum.Web3
import           System.Environment      (lookupEnv)

data LndrProvider

instance Provider LndrProvider where
    rpcUri = liftIO (fromMaybe (error errorMsg) <$> lookupEnv web3ProviderEnvVariable)
        where errorMsg = web3ProviderEnvVariable ++ " env variable not set."

runLndrWeb3 :: MonadIO m => Web3 LndrProvider a -> m (Either Web3Error a)
runLndrWeb3 = runWeb3'
