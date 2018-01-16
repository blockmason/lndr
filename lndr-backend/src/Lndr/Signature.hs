module Lndr.Signature where

import           Data.Hashable
import           Data.Text (Text)
import           Network.Ethereum.Web3.Address

type Signature = Text

class Hashable a => VerifiableSignature a where
     recoverSigner :: a -> Signature -> Address
