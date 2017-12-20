{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Friend where

import           Control.Monad.Reader
import           Data.Pool (withResource)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Lndr.Db as Db
import           Lndr.Handler.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant.API


nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler (NickRequest addr nick sig) = do
    -- TODO verify signature
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
