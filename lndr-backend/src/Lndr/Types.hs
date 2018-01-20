{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lndr.Types
    (
    -- * server configuration
      ServerState(..)
    , ServerConfig(..)

    -- * lndr api types
    , NickRequest(NickRequest, nickRequestSignature)
    , NickInfo(..)
    -- TODO clean this up, very unorganized as is
    , CreditRecord( CreditRecord, hash, creditor, debtor, submitter, signature
                  , nonce, settlementAmount
                  )
    , IssueCreditLog(IssueCreditLog, ucac, amount)
    , SettlementData(SettlementData)
    , SettlementsResponse(..)
    , VerifySettlementRequest(..)
    , RejectRequest(..)
    , Nonce(..)

    -- * push notifications-relatd types
    , PushRequest(..)
    , Notification(..)
    , NotificationAction(..)
    , DevicePlatform(..)

    -- * network statistics api response types
    , EthereumPrice(..)
    , GasStationResponse(..)
    , ConfigResponse(..)
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Lens (over, _head)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Char (toLower)
import qualified Data.Configurator.Types as Conf
import           Data.Either.Combinators (mapLeft, fromRight)
import           Data.Hashable
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Ethereum.Web3.Address (Address)
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant.API

instance Conf.Configured Address where
    convert (Conf.String x) = Just . fromRight (error "bad address") . Addr.fromText $ x
    convert _ = Nothing

instance Hashable Address where
    hashWithSalt x = hashWithSalt x . Addr.toText

instance ToHttpApiData Address where
  toUrlPiece = Addr.toText

instance FromHttpApiData Address where
  parseUrlPiece = mapLeft T.pack . Addr.fromText

newtype Nonce = Nonce { unNonce :: Integer } deriving (Show, Generic)

instance ToJSON Nonce where
    toJSON (Nonce x) = toJSON x

data SettlementData = SettlementData { settlementAmount :: Integer
                                     , settlementCurrency :: Text
                                     , settlementBlocknumber :: Integer
                                     }

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
                                 , settlementAmount :: Maybe Integer
                                 , settlementCurrency :: Maybe Text
                                 , settlementBlocknumber :: Maybe Integer
                                 } deriving (Show, Generic)
$(deriveJSON (defaultOptions { omitNothingFields = True }) ''CreditRecord)

data RejectRequest = RejectRequest { rejectRequestHash :: Text
                                   , rejectRequestSignature :: Text
                                   }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 13 }) ''RejectRequest)

data NickRequest = NickRequest { nickRequestAddr :: Address
                               , nickRequestNick :: Text
                               , nickRequestSignature :: Text
                               }
$(deriveJSON defaultOptions ''NickRequest)

data NickInfo = NickInfo { addr :: Address
                         , nick :: Text
                         } deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''NickInfo)

data PushRequest = PushRequest { pushRequestChannelID :: Text
                               , pushRequestPlatform :: Text
                               , pushRequestAddress :: Address
                               , pushRequestSignature :: Text
                               }
$(deriveJSON defaultOptions ''PushRequest)

-- The 'NotificationAction' type enumerates those events upon which a push
-- notification may be sent.
data NotificationAction = NewPendingCredit
                        | CreditConfirmation
                        | PendingCreditRejection
                        deriving Show
$(deriveJSON defaultOptions ''NotificationAction)

-- The 'DevicePlatform' type is used to select among mobile platforms in the
-- 'Notification' type below and when dealing with Urban Airship-related types
-- generally.
data DevicePlatform = Ios
                    | Android
                    deriving Show

instance ToJSON DevicePlatform where
   toJSON Ios = String "ios"
   toJSON Android = String "android"

-- This should probably be called 'Push Object' to match the Urban Airship docs
-- here: https://docs.urbanairship.com/api/ua/#push-object
data Notification = Notification { channelID :: Text
                                 , platform :: DevicePlatform
                                 , message :: Text
                                 , action :: NotificationAction
                                 } deriving Show

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

-- 'ConfigResponse' contains all the server data that users have access to via
-- the /config endpoint. By and large, this endpoint is used by clients to
-- ensure their configuratoins match the server's.
data ConfigResponse = ConfigResponse { configResponseLndrAddress :: Address
                                     , configResponseCreditProtocolAddress :: Address
                                     }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 14 }) ''ConfigResponse)

data SettlementsResponse = SettlementsResponse { unilateralSettlements :: [CreditRecord]
                                               , bilateralSettlements :: [CreditRecord]
                                               }
$(deriveJSON defaultOptions ''SettlementsResponse)

data ServerState = ServerState { dbConnectionPool :: Pool Connection
                               , serverConfig :: TVar ServerConfig
                               }

data GasStationResponse = GasStationResponse { safeLow :: Double
                                             , safeLowWait :: Double
                                             } deriving Show
$(deriveJSON defaultOptions ''GasStationResponse)

-- A newtype wrapper is used for this 'Double' value which holds an Ethereum
-- price expressed in USD. This is necessary in order to have easy decoding
-- from the JSON response of the coinbase API.
newtype EthereumPrice = EthereumPrice { unPrice :: Double } deriving (Show, Generic)

instance FromJSON EthereumPrice where
        parseJSON (Object v) = do
            dataObject <- v .: "data"
            ratesObject <- dataObject .: "rates"
            EthereumPrice . read <$> ratesObject .: "USD"

data VerifySettlementRequest = VerifySettlementRequest { verifySettlementRequestCreditHash :: Text
                                                       , verifySettlementRequestTxHash :: Text
                                                       , verifySettlementRequestCreditorAddress :: Address
                                                       , verifySettlementRequestSignature :: Text
                                                       }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 23 }) ''VerifySettlementRequest)
