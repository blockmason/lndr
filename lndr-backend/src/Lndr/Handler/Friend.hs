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


nickSearchHandler :: Text -> LndrHandler Address
nickSearchHandler nick = do
    nickMapping <- nickMap <$> ask
    assocs <- liftIO . atomically . toList $ Map.stream nickMapping
    return . fst . head . dropWhile ((/= nick) . snd) $ assocs


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


lookupFriends :: Address -> Map.Map Address [Address] -> LndrHandler [Address]
lookupFriends x y = fmap (fromMaybe []) . liftIO . atomically $ Map.lookup x y
