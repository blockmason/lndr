{-# LANGUAGE OverloadedStrings #-}

module Lndr.Handler.Friend where

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.List ((\\), nub)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           ListT (toList)
import           Lndr.Handler.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant.API
import qualified STMContainers.Bimap as Bimap
import qualified STMContainers.Map as Map

-- TODO nicks should be unique
nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler (NickRequest addr nick sig) = do
    -- TODO verify signature
    nickMapping <- nickMap <$> ask
    liftIO . atomically $ Bimap.insert1 nick addr nickMapping
    return NoContent


nickLookupHandler :: Address -> LndrHandler Text
nickLookupHandler addr = do
    nickMapping <- nickMap <$> ask
    ioMaybeToLndr "addr not found in nick db" . atomically $ Bimap.lookup1 addr nickMapping


nickSearchHandler :: Text -> LndrHandler [NickInfo]
nickSearchHandler nick = do
    nickMapping <- nickMap <$> ask
    ioMaybeToLndr "addr not found in nick db" . atomically $ fmap ((:[]) . (`NickInfo` nick)) <$> Bimap.lookup2 nick nickMapping


friendHandler :: Address -> LndrHandler [NickInfo]
friendHandler addr = do
    (ServerState _ nickMapping friendListMapping) <- ask
    lookupFriendsWithNick addr friendListMapping nickMapping


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler address adds = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    -- TODO fix this once long-term data structures are in place
    friendList <- lookupFriends address friendListMapping
    liftIO . atomically $ Map.insert (nub $ friendList ++ adds) address friendListMapping
    return NoContent


removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler address removes = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    friendList <- lookupFriends address friendListMapping
    liftIO . atomically $ Map.insert (friendList \\ removes) address friendListMapping
    return NoContent


lookupFriends :: Address -> Map.Map Address [Address] -> LndrHandler [Address]
lookupFriends x y = fmap (fromMaybe []) . liftIO . atomically $ Map.lookup x y

lookupFriendsWithNick :: Address -> Map.Map Address [Address] -> Bimap.Bimap Address Text -> LndrHandler [NickInfo]
lookupFriendsWithNick x y z = do
    friends <- lookupFriends x y
    liftIO . atomically $ mapM toNickInfo friends
    where
        toNickInfo x = NickInfo x . fromMaybe "N/A" <$> Bimap.lookup1 x z
