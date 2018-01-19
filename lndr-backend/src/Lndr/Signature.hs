module Lndr.Signature where

import           Data.Hashable
import           Data.Text (Text)
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3.Address

class Hashable a => VerifiableSignature a where
     recoverSigner :: a -> Address

instance Hashable NickRequest where
    hashWithSalt (NickRequest addr nick _) = hashWithSalt (toText addr, nick)

instance VerifiableSignature NickRequest where
    recoverSigner x = undefined

-- AddFriendRequest
-- RemoveFriendRequest

instance Hashable PushRequest where
    hashWithSalt (PushRequest channelID platform) = hashWithSalt (channelID, platform)

instance VerifiableSignature PushRequest where
    recoverSigner = undefined
