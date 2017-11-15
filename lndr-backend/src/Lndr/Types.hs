{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Lndr.Types
    ( ServerState(..)
    , NickRequest(..)
    , NickInfo(..)
    , CreditRecord(..)
    , Unsigned
    , Signed
    , IssueCreditLog(IssueCreditLog)
    , RejectRecord(..)
    , PendingRecord(..)
    , Nonce(..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Either.Combinators (mapLeft)
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.Ethereum.Web3.Address (Address)
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant.API
import qualified STMContainers.Map as Map

-- TODO remove this once Address derives Generic in hs-web3
instance Hashable Address where
    hashWithSalt x = hashWithSalt x . Addr.toText

instance ToHttpApiData Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

newtype Nonce = Nonce { mkNonce :: Integer } deriving (Show, Generic)
instance ToJSON Nonce where
    toJSON (Nonce x) = toJSON x

data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Address
                                     , debtor :: Address
                                     , amount :: Integer
                                     , memo :: Text
                                     } deriving Show
$(deriveJSON defaultOptions ''IssueCreditLog)

-- Types that populate `CreditRecord`'s phantom type field
data Signed = Signed
$(deriveJSON defaultOptions ''Signed)
data Unsigned = Unsigned
$(deriveJSON defaultOptions ''Unsigned)

-- `a` is a phantom type that indicates whether a record has been signed or not
data CreditRecord a = CreditRecord { creditor :: Text
                                   , debtor :: Text
                                   , amount :: Integer
                                   , memo :: Text
                                   , signature :: Text
                                   } deriving (Show, Generic)
$(deriveJSON defaultOptions ''CreditRecord)

data PendingRecord = PendingRecord { creditRecord :: CreditRecord Signed
                                   , submitter :: Address
                                   , nonce :: Integer
                                   , hash :: Text
                                   } deriving (Show, Generic)
$(deriveJSON defaultOptions ''PendingRecord)


data RejectRecord = RejectRecord { rejectSig :: Text
                                 , hash :: Text
                                 }
$(deriveJSON defaultOptions ''RejectRecord)

data NickRequest = NickRequest { addr :: Address
                               , nick :: Text
                               , sig :: Text
                               }
$(deriveJSON defaultOptions ''NickRequest)

data NickInfo = NickInfo { addr :: Address
                         , nick :: Text
                         } deriving Show
$(deriveJSON defaultOptions ''NickInfo)

data ServerState = ServerState { pendingMap :: Map.Map Text PendingRecord
                               , nickMap :: Map.Map Address Text
                               , friendlistMap :: Map.Map Address [Address]
                               }
