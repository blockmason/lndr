{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Friendships where

import           Database.PostgreSQL.Simple
import           Lndr.Types
import           Lndr.Db.Types
import           Network.Ethereum.Web3

addFriends :: [(Address, Address)] -> Connection -> IO Int
addFriends addressPairs conn = fromIntegral <$>
    executeMany conn "INSERT INTO friendships (origin, friend) VALUES (?,?) ON CONFLICT ON CONSTRAINT friendships_origin_friend_key DO NOTHING" addressPairs


removeFriends :: Address -> [Address] -> Connection -> IO Int
removeFriends addr addresses conn = fromIntegral <$>
    execute conn "DELETE FROM friendships WHERE origin = ? AND friend in ? OR friend = ? AND origin in ?" (addr, In addresses, addr, In addresses)


lookupFriends :: Address -> Connection -> IO [UserInfo]
lookupFriends addr conn =
    query conn "SELECT DISTINCT inbound.origin, nicknames.nickname FROM friendships inbound INNER JOIN friendships outbound ON inbound.friend = outbound.origin AND inbound.origin = outbound.friend LEFT JOIN nicknames ON nicknames.address = inbound.origin WHERE inbound.friend = ?" (Only addr) :: IO [UserInfo]


lookupFriendRequests :: Address -> Connection -> IO [UserInfo]
lookupFriendRequests addr conn =
    query conn "SELECT DISTINCT inbound.origin, nicknames.nickname FROM friendships inbound LEFT JOIN friendships outbound ON inbound.friend = outbound.origin AND inbound.origin = outbound.friend LEFT JOIN nicknames ON nicknames.address = inbound.origin WHERE inbound.friend = ? AND outbound.friend IS NULL" (Only addr) :: IO [UserInfo]
