module Lndr.Handler.Friend where

import           Control.Monad.Reader
import           Control.Concurrent.STM
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


friendHandler :: Address -> LndrHandler [Address]
friendHandler addr = do
    friendListMapping <- friendlistMap <$> ask
    fmap (fromMaybe []) . liftIO . atomically $ Map.lookup addr friendListMapping


addFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
addFriendsHandler addr removes = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    -- TODO use same function as friendHandler, then modify list, then update
    -- the mapping
    return NoContent

removeFriendsHandler :: Address -> [Address] -> LndrHandler NoContent
removeFriendsHandler addr adds = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    -- TODO use same function as friendHandler, then modify list, then update
    -- the mapping
    return NoContent
