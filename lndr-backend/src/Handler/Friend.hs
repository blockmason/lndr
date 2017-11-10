module Handler.Friend where

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Maybe (fromMaybe)
import           Handler.Types
import           Network.Ethereum.Web3
import           Servant.API
import qualified STMContainers.Map as Map
import           Types

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


updateFriendsHandler :: Address -> UpdateFriendsRequest -> LndrHandler NoContent
updateFriendsHandler addr (UpdateFriendsRequest adds removes) = do
    -- TODO verify signature
    friendListMapping <- friendlistMap <$> ask
    -- TODO use same function as friendHandler, then modify list, then update
    -- the mapping
    return NoContent
