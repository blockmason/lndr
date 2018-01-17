module Lndr.Signature where

import           Data.Hashable
import           Data.Text (Text)
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3.Address

class Hashable a => VerifiableSignature a where
     recoverSigner :: a -> Address

instance Hashable NickRequest where
    hashWithSalt = undefined

instance VerifiableSignature NickRequest where
    recoverSigner x = undefined

-- AddFriendRequest
-- RemoveFriendRequest

instance Hashable PushRequest where
    hashWithSalt = undefined

instance VerifiableSignature PushRequest where
    recoverSigner = undefined
