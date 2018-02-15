{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Friend where

import qualified Aws
import qualified Aws.S3 as S3
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import           Data.Pool             (withResource)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Lndr.Db               as Db
import           Lndr.Handler.Types
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3
import           Network.HTTP.Client
import           Servant
import           Text.EmailAddress


nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler r@(NickRequest addr nick sig) = do
    unless (Right addr == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool . Db.insertNick addr $ T.toLower nick
    return NoContent


nickLookupHandler :: Address -> LndrHandler Text
nickLookupHandler addr = do
    pool <- dbConnectionPool <$> ask
    ioMaybeToLndr "addr not found in nick db" . withResource pool $ Db.lookupNick addr


nickSearchHandler :: Text -> LndrHandler [NickInfo]
nickSearchHandler nick = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool . Db.lookupAddressesByFuzzyNick $ T.toLower nick


-- TODO remove when will says so
nickTakenHandler :: Text -> LndrHandler Bool
nickTakenHandler nick = do
    pool <- dbConnectionPool <$> ask
    liftIO . fmap (not . null) . withResource pool . Db.lookupAddressByNick $ T.toLower nick


friendHandler :: Address -> LndrHandler [NickInfo]
friendHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.lookupFriendsWithNick addr


userHandler :: Maybe EmailAddress -> Maybe Nick -> LndrHandler NickInfo
userHandler (Just email) _ = do
    pool <- dbConnectionPool <$> ask
    nickInfoM <- liftIO . withResource pool . Db.lookupAddressByEmail $ email
    case nickInfoM of
        Just nickInfo -> return nickInfo
        Nothing -> throwError (err404 {errBody = "No corresponding user found based on provided email."})
userHandler _ (Just nick) = do
    pool <- dbConnectionPool <$> ask
    nickInfoM <- liftIO . withResource pool . Db.lookupAddressByNick $ T.toLower nick
    case nickInfoM of
        Just nickInfo -> return nickInfo
        Nothing -> throwError (err404 {errBody = "No corresponding user found based on provided nick."})
userHandler Nothing Nothing = throwError (err400 {errBody = "No identifying information specified."})


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler address adds = do
    -- TODO verify signature
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.addFriends address adds
    return NoContent


removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler address removes = do
    -- TODO verify signature
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.removeFriends address removes
    return NoContent


emailHandler :: EmailRequest -> LndrHandler NoContent
emailHandler r@(EmailRequest addr email sig) = do
    unless (Right addr == recoverSigner r) $ throwError (err401 {errBody = "Bad signature."})
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool . Db.insertEmail addr $ toText email
    return NoContent


emailLookupHandler :: Address -> LndrHandler EmailAddress
emailLookupHandler addr = do
    pool <- dbConnectionPool <$> ask
    ioMaybeToLndr "addr not found in nick db" . withResource pool $ Db.lookupEmail addr


awsConfig :: IO ( Aws.Configuration
                , S3.S3Configuration Aws.NormalQuery
                )
awsConfig = do  -- Set up AWS credentials and the default configuration
    cfg <- Aws.baseConfiguration
    let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
    return (cfg, s3cfg)


photoUploadHandler :: ProfilePhotoRequest -> LndrHandler NoContent
photoUploadHandler r@(ProfilePhotoRequest photo sig) =
    do let address = recoverSigner r
           elementName = stripHexPrefix . T.pack $ show address ++ ".jpeg"
           body = RequestBodyBS . B64.decodeLenient $ T.encodeUtf8 photo
       (cfg, s3cfg) <- liftIO awsConfig
       Aws.simpleAws cfg s3cfg $ S3.putObject "lndr-avatars" elementName body
       return NoContent
