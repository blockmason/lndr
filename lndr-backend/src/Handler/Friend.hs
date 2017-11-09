module Handler.Friend where

import           Handler.Types
import           Network.Ethereum.Web3
import           Servant.API
import           Types

nickHandler :: NickRequest -> LndrHandler NoContent
nickHandler _ = undefined


friendHandler :: Address -> LndrHandler [Address]
friendHandler _ = undefined


updateFriendsHandler :: Address -> UpdateFriendsRequest -> LndrHandler NoContent
updateFriendsHandler _ _ = undefined
