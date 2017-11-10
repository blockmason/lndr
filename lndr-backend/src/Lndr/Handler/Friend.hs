module Lndr.Handler.Friend where

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.List ((\\), nub)
import           Data.Maybe (fromMaybe)
import           Lndr.Handler.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant.API
import qualified STMContainers.Map as Map

nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler (NickRequest addr nick sig) = do
    -- TODO verify signature
    nickMapping <- nickMap <$> ask
    liftIO . atomically $ Map.insert nick addr nickMapping
    return NoContent


lookupFriends :: Address -> Map.Map Address [Address] -> LndrHandler [Address]
lookupFriends x y = fmap (fromMaybe []) . liftIO . atomically $ Map.lookup x y


friendHandler :: Address -> LndrHandler [Address]
friendHandler addr = do
    friendListMapping <- friendlistMap <$> ask
    lookupFriends addr friendListMapping


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler addr adds = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    friendList <- lookupFriends addr friendListMapping
    liftIO . atomically $ Map.insert (nub $ friendList ++ adds) addr friendListMapping
    return NoContent

removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler addr removes = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    friendList <- lookupFriends addr friendListMapping
    liftIO . atomically $ Map.insert (friendList \\ removes) addr friendListMapping
    return NoContent
