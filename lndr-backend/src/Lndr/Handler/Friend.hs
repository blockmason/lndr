{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Handler.Friend where

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
import           Lndr.Notifications
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import qualified Network.AWS              as Aws
import qualified Network.AWS.S3.PutObject as Aws
import qualified Network.AWS.S3.Types     as Aws
import           Network.Ethereum.Web3
import           Network.HTTP.Client
import           Servant
import           Text.EmailAddress
import           System.Log.FastLogger


nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler r@(NickRequest addr nick sig) = do
    unless (Right addr == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})
    pool <- asks dbConnectionPool
    liftIO . withResource pool . Db.insertNick addr $ T.toLower nick
    pure NoContent


nickLookupHandler :: Address -> LndrHandler Text
nickLookupHandler addr = do
    pool <- asks dbConnectionPool
    ioMaybeToLndr "addr not found in nick db" . withResource pool $ Db.lookupNick addr


nickSearchHandler :: Text -> LndrHandler [UserInfo]
nickSearchHandler nick = do
    pool <- asks dbConnectionPool
    liftIO . withResource pool . Db.lookupAddressesByFuzzyNick $ T.toLower nick


friendHandler :: Address -> LndrHandler [UserInfo]
friendHandler addr = do
    pool <- asks dbConnectionPool
    liftIO . withResource pool $ Db.lookupFriends addr


friendRequestsHandler :: Address -> LndrHandler [UserInfo]
friendRequestsHandler address = do
    pool <- asks dbConnectionPool
    liftIO . withResource pool $ Db.lookupFriendRequests address


userHandler :: Maybe EmailAddress -> Maybe Nick -> LndrHandler UserInfo
userHandler (Just email) _ = do
    pool <- asks dbConnectionPool
    nickInfoM <- liftIO . withResource pool . Db.lookupAddressByEmail $ email
    case nickInfoM of
        Just nickInfo -> pure nickInfo
        Nothing -> throwError (err404 {errBody = "No corresponding user found based on provided email."})
userHandler _ (Just nick) = do
    pool <- asks dbConnectionPool
    nickInfoM <- liftIO . withResource pool . Db.lookupAddressByNick $ T.toLower nick
    case nickInfoM of
        Just nickInfo -> pure nickInfo
        Nothing -> throwError (err404 {errBody = "No corresponding user found based on provided nick."})
userHandler Nothing Nothing = throwError (err400 {errBody = "No identifying information specified."})


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler address friendAddresses = do
    -- TODO verify signature
    (ServerState pool configTVar loggerSet) <- ask
    config <- liftIO $ readTVarIO configTVar
    liftIO . withResource pool $ Db.addFriends ((address,) <$> friendAddresses)
    
    let attemptToNotify friendAddress = do
            pendingRequests <- liftIO . withResource pool $ Db.lookupPendingRequest address friendAddress
            when (pendingRequests > 0) $ do
                pushDataM <- liftIO . withResource pool $ Db.lookupPushDatumByAddress friendAddress
                nicknameM <- liftIO . withResource pool $ Db.lookupNick address

                forM_ pushDataM $ \(channelID, platform) -> liftIO $ do
                    responseCode <- sendNotification config (Notification channelID platform nicknameM NewFriendRequest)
                    let logMsg = "Notification response (" ++ (show friendAddress) ++ "): " ++ show responseCode
                    liftIO $ pushLogStrLn loggerSet . toLogStr $ logMsg
    
    forM_ friendAddresses attemptToNotify

    pure NoContent


removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler address removes = do
    -- TODO verify signature
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.removeFriends address removes
    pure NoContent


emailHandler :: EmailRequest -> LndrHandler NoContent
emailHandler r@(EmailRequest addr email sig) = do
    unless (Right addr == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})
    pool <- asks dbConnectionPool
    liftIO . withResource pool . Db.insertEmail addr $ toText email
    pure NoContent


emailLookupHandler :: Address -> LndrHandler EmailAddress
emailLookupHandler addr = do
    pool <- asks dbConnectionPool
    ioMaybeToLndr "addr not found in nick db" . withResource pool $ Db.lookupEmail addr


photoUploadHandler :: ProfilePhotoRequest -> LndrHandler NoContent
photoUploadHandler r@(ProfilePhotoRequest photo sig) = do
    configTVar <- asks serverConfig
    config <- liftIO $ readTVarIO configTVar
    let Right address = recoverSigner r
        elementName = Aws.ObjectKey . stripHexPrefix . T.pack $ show address ++ ".jpeg"
        body = Aws.toBody . B64.decodeLenient $ T.encodeUtf8 photo
        accessKeyId = awsAccessKeyId config
        secretAccessKey = awsSecretAccessKey config
        bucket = Aws.BucketName $ awsPhotoBucket config
    env <- liftIO . Aws.newEnv $ Aws.FromKeys (Aws.AccessKey accessKeyId) (Aws.SecretKey secretAccessKey)
    liftIO . runResourceT . Aws.runAWS env . Aws.within Aws.Oregon $
        Aws.send (set Aws.poACL (Just Aws.OPublicRead) $ Aws.putObject bucket elementName body)
    pure NoContent
