{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Handler.Identity where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as LBS
import           Data.Pool               (withResource)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Lndr.Db                 as Db
import           Lndr.Handler.Types
import           Lndr.IdentityVerification
import           Lndr.Signature
import           Lndr.Types              as Types
import           Lndr.Util
import           Network.Ethereum.Web3
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           Servant
import           Text.EmailAddress
import           System.Log.FastLogger

verifyIdentityHandler :: IdentityVerificationRequest -> LndrHandler NoContent
verifyIdentityHandler req@(IdentityVerificationRequest email addr info signature) = do
  (ServerState pool configTVar loggerSet) <- ask
  config <- liftIO $ readTVarIO configTVar
  
  response <- liftIO $ sendVerificationRequest config req
  
  -- store in db
  liftIO . withResource pool . Db.addVerificationStatus $ VerificationStatusEntry addr (Types.id response) ""

  pure NoContent


verifyIdentityCallbackHandler :: IdentityVerificationStatus -> LndrHandler NoContent
verifyIdentityCallbackHandler status@(IdentityVerificationStatus applicantId _ _ addr _ _ _ rev) = do
  (ServerState pool configTVar loggerSet) <- ask
  liftIO . withResource pool . Db.addVerificationStatus $ VerificationStatusEntry addr applicantId $ reviewAnswer rev
  pure NoContent


checkIdentityVerificationHandler :: VerificationStatusRequest -> LndrHandler VerificationStatusEntry
checkIdentityVerificationHandler req@(VerificationStatusRequest addr sig) = do
  (ServerState pool configTVar loggerSet) <- ask
  config <- liftIO $ readTVarIO configTVar
  -- check db for status entry
  status <- liftIO . withResource pool . Db.lookupVerificationStatus $ addr

  case status of
    -- if there is no status, query the API
    Just idenStat@(VerificationStatusEntry _ _ "") -> do
        response <- liftIO $ getVerificationStatus config $ sumsubId idenStat

        let newStatus = (idenStat { status = (reviewAnswer $ Types.review response) })
    
        liftIO . withResource pool . Db.addVerificationStatus $ newStatus
        return newStatus

    -- if there is a status, return it
    Just idenStat -> do
        return idenStat

    -- if there is no status, return a blank
    Nothing -> do
        return $ VerificationStatusEntry addr "" ""
    