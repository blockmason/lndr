{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Handler.Identity where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import qualified Data.ByteString.Char8   as BS8
import           Data.Pool               (withResource)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding      as TE
import qualified Data.Aeson              as A
import qualified Data.HexString          as H
import           Data.Maybe              (fromMaybe)
import qualified Crypto.Hash.SHA1        as SHA
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
verifyIdentityHandler req@(IdentityVerificationRequest _ addr _ _ _ idDocs) = do
  unless (Right addr == recoverSigner req) $ throwError (err401 {errBody = "Bad signature."})
  (ServerState pool configTVar loggerSet) <- ask
  config <- liftIO $ readTVarIO configTVar
  response <- liftIO $ sendVerificationRequest config req

  liftIO . mapM (\doc -> sendVerificationDocument loggerSet config (Types.id response) doc) $ fromMaybe [] idDocs
  
  -- store in db
  liftIO . withResource pool . Db.addVerificationStatus $ VerificationStatusEntry addr (Types.id response) ""

  pure NoContent


verifyIdentityCallbackHandler :: Maybe Text -> LT.Text -> LndrHandler NoContent
verifyIdentityCallbackHandler Nothing _ = do
  throwError (err401 {errBody = "No digest parameter."})
verifyIdentityCallbackHandler (Just digest) raw = do
  (ServerState pool configTVar loggerSet) <- ask
  config <- liftIO $ readTVarIO configTVar

  let jsonData = TL.encodeUtf8 raw
      status = A.decode jsonData :: Maybe IdentityVerificationStatus

  liftIO $ do pushLogStrLn loggerSet . toLogStr $ "STATUS   " ++ show status
  liftIO $ do pushLogStrLn loggerSet . toLogStr $ "JSON   " ++ show raw

  case status of
    Just stat@(IdentityVerificationStatus applicantId _ _ _ addr _ _ _ rev) -> do
      let computedDigest = TE.decodeUtf8 $ B16.encode $ SHA.hmac (BS8.pack $ sumsubApiCallbackSecret config) (LBS.toStrict jsonData)
      unless (digest == computedDigest) $ throwError (err401 {errBody = "Digest parameter does not match message body."})

      liftIO . withResource pool . Db.addVerificationStatus $ VerificationStatusEntry addr applicantId $ reviewAnswer rev
      pure NoContent
    
    _ -> do
      throwError (err400 {errBody = "Request Body is in the wrong format."})


checkIdentityVerificationHandler :: VerificationStatusRequest -> LndrHandler VerificationStatusEntry
checkIdentityVerificationHandler req@(VerificationStatusRequest addr sig) = do
  unless (Right addr == recoverSigner req) $ throwError (err401 {errBody = "Bad signature."})
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
    