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
import           Network.Ethereum.Web3
import           Servant


registerPushHandler :: PushRequest -> LndrHandler NoContent
registerPushHandler r@(PushRequest channelID platform addr _) = do
    unless (Right addr == recoverSigner r) $ throwError (err400 {errBody = "Bad signature."})
    pool <- asks dbConnectionPool
    liftIO . withResource pool $ Db.insertPushDatum addr channelID platform
    pure NoContent


configHandler :: LndrHandler ConfigResponse
configHandler = do
    configTVar <- asks serverConfig
    configToResponse <$> liftIO (readTVarIO configTVar)
