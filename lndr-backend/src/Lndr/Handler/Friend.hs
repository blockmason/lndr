{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Friend where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Pool             (withResource)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Lndr.Db               as Db
import           Lndr.Handler.Types
import           Lndr.Signature
import           Lndr.Types
import           Network.Ethereum.Web3
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


nickTakenHandler :: Text -> LndrHandler Bool
nickTakenHandler nick = do
    pool <- dbConnectionPool <$> ask
    liftIO . fmap (not . null) . withResource pool . Db.lookupAddressByNick $ T.toLower nick


friendHandler :: Address -> LndrHandler [NickInfo]
friendHandler addr = do
    pool <- dbConnectionPool <$> ask
    liftIO . withResource pool $ Db.lookupFriendsWithNick addr


userHandler :: Maybe EmailAddress -> LndrHandler NickInfo
userHandler (Just email) = do
    pool <- dbConnectionPool <$> ask
    nickInfoM <- liftIO . withResource pool . Db.lookupAddressByEmail $ email
    case nickInfoM of
        Just nickInfo -> return nickInfo
        Nothing -> throwError (err404 {errBody = "No corresponding user found."})
userHandler Nothing = throwError (err400 {errBody = "No identifying information specified."})


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


emailTakenHandler :: EmailAddress -> LndrHandler Bool
emailTakenHandler email = do
    pool <- dbConnectionPool <$> ask
    liftIO . fmap (not . null) . withResource pool . Db.lookupAddressByEmail $ email
