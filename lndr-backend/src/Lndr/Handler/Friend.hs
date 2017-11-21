{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Friend where

import           Control.Monad.Reader
import           Data.List ((\\), nub)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Lndr.Db as Db
import           ListT (toList)
import           Lndr.Handler.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant.API


nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler (NickRequest addr nick sig) = do
    conn <- dbConnection <$> ask
    liftIO $ Db.insertNick conn addr nick
    return NoContent


nickLookupHandler :: Address -> LndrHandler Text
nickLookupHandler addr = do
    conn <- dbConnection <$> ask
    ioMaybeToLndr "addr not found in nick db" $ Db.lookupNick conn addr


nickSearchHandler :: Text -> LndrHandler [NickInfo]
nickSearchHandler nick = do
    conn <- dbConnection <$> ask
    liftIO $ Db.lookupAddresByNick conn nick


friendHandler :: Address -> LndrHandler [NickInfo]
friendHandler addr = do
    conn <- dbConnection <$> ask
    -- TODO currently won't return friends who don't have nicks. Is this
    -- behavior acceptable?
    liftIO $ Db.lookupFriendsWithNick conn addr


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler address adds = do
    -- TODO verify signature
    conn <- dbConnection <$> ask
    -- TODO check that return SUCCESS
    liftIO $ Db.addFriends conn address adds
    return NoContent


removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler address removes = do
    -- TODO verify signature
    conn <- dbConnection <$> ask
    -- TODO check that return SUCCESS
    liftIO $ Db.removeFriends conn address removes
    return NoContent
