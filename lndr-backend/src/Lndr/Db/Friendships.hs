{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lndr.Db.Friendships where

import           Database.PostgreSQL.Simple
import           Lndr.Types
import           Lndr.Db.Types
import           Network.Ethereum.Web3

addFriends :: Address -> [Address] -> Connection -> IO Int
addFriends addr addresses conn = fromIntegral <$>
    executeMany conn "INSERT INTO friendships (origin, friend) VALUES (?,?) ON CONFLICT ON CONSTRAINT friendships_origin_friend_key DO NOTHING" ((addr,) <$> addresses)


removeFriends :: Address -> [Address] -> Connection -> IO Int
removeFriends addr addresses conn = fromIntegral <$>
    execute conn "DELETE FROM friendships WHERE origin = ? AND friend in ?" (addr, In addresses)


lookupFriends :: Address -> Connection -> IO [Address]
lookupFriends addr conn = fmap fromOnly <$>
    (query conn "SELECT friend FROM friendships WHERE origin = ?" (Only addr) :: IO [Only Address])


lookupFriendsWithNick :: Address -> Connection -> IO [NickInfo]
lookupFriendsWithNick addr conn =
    query conn "SELECT friend, nickname FROM friendships, nicknames WHERE origin = ? AND address = friend" (Only addr) :: IO [NickInfo]
