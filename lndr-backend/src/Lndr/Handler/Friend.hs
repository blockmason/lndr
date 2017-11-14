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
import qualified STMContainers.Map as Map

-- TODO nicks should be unique
nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler (NickRequest addr nick sig) = do
    -- TODO verify signature
    nickMapping <- nickMap <$> ask
    liftIO . atomically $ Map.insert nick addr nickMapping
    return NoContent


nickLookupHandler :: Address -> LndrHandler Text
nickLookupHandler addr = do
    nickMapping <- nickMap <$> ask
    ioMaybeToLndr "addr not found in nick db" . atomically $ Map.lookup addr nickMapping


nickSearchHandler :: Text -> LndrHandler [NickInfo]
nickSearchHandler nick = do
    nickMapping <- nickMap <$> ask
    assocs <- liftIO . atomically . toList $ Map.stream nickMapping
    return . (: []) . uncurry NickInfo . head . dropWhile ((/= nick) . snd) $ assocs


friendHandler :: Address -> LndrHandler [NickInfo]
friendHandler addr = do
    friendListMapping <- friendlistMap <$> ask
    lookupFriends addr friendListMapping


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler address adds = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    friendList <-  fmap (\(NickInfo addr _) -> addr) <$> lookupFriends address friendListMapping
    liftIO . atomically $ Map.insert (nub $ friendList ++ adds) address friendListMapping
    return NoContent


removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler address removes = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    friendList <- fmap (\(NickInfo addr _) -> addr) <$> lookupFriends address friendListMapping
    liftIO . atomically $ Map.insert (friendList \\ removes) address friendListMapping
    return NoContent


lookupFriends :: Address -> Map.Map Address [Address] -> LndrHandler [NickInfo]
lookupFriends x y = fmap (fmap (`NickInfo` "N/A") . fromMaybe []) . liftIO . atomically $ Map.lookup x y
