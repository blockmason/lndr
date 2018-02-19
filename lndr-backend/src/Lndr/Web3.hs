module Lndr.Web3 where

import           Control.Monad.IO.Class
import           Lndr.Config
import           Lndr.Types
import           Network.Ethereum.Web3

data LndrProvider

-- TODO eliminate the need to go to disk before each request to the blockchain
-- LOOKUP ENV VAR?
instance Provider LndrProvider where
    rpcUri = liftIO $ do
        config <- loadConfig
        return (web3Host config ++ ":" ++ web3Port config)

runLndrWeb3 :: MonadIO m => Web3 LndrProvider a -> m (Either Web3Error a)
runLndrWeb3 = runWeb3'
