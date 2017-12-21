{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lndr.Types
    ( ServerState(..)
    , ServerConfig(..)
    , NickRequest(..)
    , NickInfo(..)
    , PushRequest(..)
    , CreditRecord(CreditRecord, hash, creditor, debtor, submitter, signature, nonce)
    , IssueCreditLog(IssueCreditLog, ucac, amount)
    , RejectRecord(RejectRecord)
    , Nonce(..)
    , GasStationResponse(..)
    , Notification(..)
    , DevicePlatform(..)
    ) where

import           Control.Concurrent.STM.TVar
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapLeft)
import           Data.Hashable
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Ethereum.Web3.Address (Address)
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant.API

instance Hashable Address where
    hashWithSalt x = hashWithSalt x . Addr.toText

instance ToHttpApiData Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

newtype Nonce = Nonce { unNonce :: Integer } deriving (Show, Generic)
instance ToJSON Nonce where
    toJSON (Nonce x) = toJSON x

data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Address
                                     , debtor :: Address
                                     , amount :: Integer
                                     , nonce :: Integer
                                     , memo :: Text
                                     } deriving (Show, Generic)
$(deriveJSON defaultOptions ''IssueCreditLog)

instance Eq IssueCreditLog where
    (==) (IssueCreditLog u1 c1 d1 a1 n1 _) (IssueCreditLog u2 c2 d2 a2 n2 _) =
            u1 == u2 && c1 == c2 && d1 == d2 && a1 == a2 && n1 == n2

-- `a` is a phantom type that indicates whether a record has been signed or not
data CreditRecord = CreditRecord { creditor :: Address
                                 , debtor :: Address
                                 , amount :: Integer
                                 , memo :: Text
                                 , submitter :: Address
                                 , nonce :: Integer
                                 , hash :: Text
                                 , signature :: Text
                                 } deriving (Show, Generic)
$(deriveJSON defaultOptions ''CreditRecord)

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
                         } deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''NickInfo)

data PushRequest = PushRequest { channelID :: Text
                               , platform :: Text
                               }
$(deriveJSON defaultOptions ''PushRequest)

data NotificationAction = NewPendingCredit
                        | CreditConfirmation
                        | PendingCreditRejection
$(deriveJSON defaultOptions ''NotificationAction)

data DevicePlatform = Ios
                    | Android

instance ToJSON DevicePlatform where
   toJSON Ios = String "ios"
   toJSON Android = String "android"

data Notification = Notification { channelID :: Text
                                 , platform :: DevicePlatform
                                 , message :: Text
                                 , action :: NotificationAction
                                 }

instance ToJSON Notification where
    toJSON (Notification channelID platform message action) =
        object [ "audience" .= object [ deviceChannel platform .= channelID ]
               , "notification" .=
                    object [ "alert" .= message
                           , "actions" .=
                                 object [ "app_defined" .=
                                             object [ "LNDR_ACTIONS" .= [ action ] ]
                                        ]
                           ]
               , "device_types" .= [ platform ]
               ]
        where deviceChannel Ios = "ios_channel"
              deviceChannel Android = "android_channel"

data ServerConfig = ServerConfig { lndrUcacAddr :: !Address
                                 , creditProtocolAddress :: !Address
                                 , issueCreditEvent :: !Text
                                 , scanStartBlock :: !Integer
                                 , dbUser :: !Text
                                 , dbUserPassword :: !Text
                                 , dbName :: !Text
                                 , executionAddress :: !Address
                                 , gasPrice :: !Integer
                                 , maxGas :: !Integer
                                 , urbanAirshipKey :: !ByteString
                                 , urbanAirshipSecret :: !ByteString
                                 }

data ServerState = ServerState { dbConnectionPool :: Pool Connection
                               , serverConfig :: TVar ServerConfig
                               }

data GasStationResponse = GasStationResponse { safeLow :: Double
                                             , safeLowWait :: Double
                                             } deriving Show
$(deriveJSON defaultOptions ''GasStationResponse)
