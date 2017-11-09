{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Either.Combinators (mapLeft)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.Ethereum.Web3.Address (Address)
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant.API
import qualified STMContainers.Map as Map

instance ToHttpApiData Addr.Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Addr.Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

newtype Nonce = Nonce { mkNonce :: Integer } deriving (Show, Generic)
$(deriveJSON defaultOptions ''Nonce)

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
                                   , hash :: Text
                                   }
$(deriveJSON defaultOptions ''PendingRecord)


data RejectRecord = RejectRecord { rejectSig :: Text
                                 , hash :: Text
                                 }
$(deriveJSON defaultOptions ''RejectRecord)

data NickRequest = NickRequest { nick :: Text
                               , sig :: Text
                               }
$(deriveJSON defaultOptions ''NickRequest)

data UpdateFriendsRequest = UpdateFriendsRequest { friendsToAdd :: [Address]
                                                 , friendsToRemove :: [Address]
                                                 }
$(deriveJSON defaultOptions ''UpdateFriendsRequest)

newtype LndrError = LndrError { unLndrError :: String } deriving (Show, Generic)
$(deriveJSON defaultOptions ''LndrError)

data ServerState = ServerState { pendingMap :: Map.Map Text PendingRecord
                               , nickMap :: Map.Map Address Text
                               , friendlistMap :: Map.Map Address [Address]
                               }
