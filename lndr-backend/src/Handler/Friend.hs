module Handler.Friend where

import           Control.Monad.Reader
import           Control.Concurrent.STM
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
friendHandler _ = undefined


updateFriendsHandler :: Address -> UpdateFriendsRequest -> LndrHandler NoContent
updateFriendsHandler _ _ = undefined
