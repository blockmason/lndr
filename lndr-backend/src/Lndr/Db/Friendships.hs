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
    query conn "SELECT inbound.origin, nicknames.nickname FROM friendships inbound INNER JOIN friendships outbound ON inbound.friend = outbound.origin AND inbound.origin = outbound.friend LEFT JOIN nicknames ON nicknames.address = inbound.origin WHERE inbound.friend = ?" (Only addr) :: IO [UserInfo]


lookupInboundFriendRequests :: Address -> Connection -> IO [UserInfo]
lookupInboundFriendRequests addr conn =
    query conn "SELECT inbound.origin, nicknames.nickname FROM friendships inbound LEFT JOIN friendships outbound ON inbound.friend = outbound.origin AND inbound.origin = outbound.friend LEFT JOIN nicknames ON nicknames.address = inbound.origin WHERE inbound.friend = ? AND outbound.friend IS NULL" (Only addr) :: IO [UserInfo]


lookupOutboundFriendRequests :: Address -> Connection -> IO [UserInfo]
lookupOutboundFriendRequests addr conn =
    query conn "SELECT outbound.friend, nicknames.nickname FROM friendships outbound LEFT JOIN friendships inbound ON outbound.friend = inbound.origin AND outbound.origin = inbound.friend LEFT JOIN nicknames ON nicknames.address = outbound.friend WHERE outbound.origin = ? AND inbound.friend IS NULL" (Only addr) :: IO [UserInfo]


sentFriendRequestTo :: Address -> [Address] -> Connection -> IO [Address]
sentFriendRequestTo addr friendAddresses conn = do
    addresses <- query conn "SELECT inbound.friend FROM friendships inbound LEFT JOIN friendships outbound ON inbound.friend = outbound.origin AND inbound.origin = outbound.friend WHERE inbound.origin = ? AND inbound.friend in ? AND outbound.friend IS NULL" (addr, In friendAddresses) :: IO [(Only Address)]
    return $ fmap (\(Only address) -> address) addresses
