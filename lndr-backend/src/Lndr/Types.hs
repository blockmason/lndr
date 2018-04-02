{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lndr.Types
    (
    -- * server configuration
      ServerState(..)
    , ServerConfig(..)

    -- * lndr api types
    , NickRequest(..)
    , EmailRequest(..)
    , UserInfo(..)
    , Nick
    , ProfilePhotoRequest(..)
    , CreditRecord(..)
    , BilateralCreditRecord(..)
    , IssueCreditLog(..)
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
    , EthereumPrices(..)
    , GasStationResponse(..)
    , ConfigResponse(..)

    -- * descriptive bytestring types
    , TransactionHash
    , CreditHash
    , Signature
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Lens                  (over, _head)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Bimap                    as B
import           Data.ByteString               (ByteString)
import           Data.Char                     (toLower)
import qualified Data.Configurator.Types       as Conf
import           Data.Default
import           Data.Either.Combinators       (fromRight, mapLeft)
import           Data.Hashable
import qualified Data.Map                      as M
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Word
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Ethereum.Web3.Address (Address)
import qualified Network.Ethereum.Web3.Address as Addr
import           Servant.API
import           System.Log.FastLogger
import           Text.EmailAddress


type TransactionHash = Text

type CreditHash = Text

type Signature = Text

type Nick = Text

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

data IssueCreditLog = IssueCreditLog { creditLogUcac     :: Address
                                     , creditLogCreditor :: Address
                                     , creditLogDebtor   :: Address
                                     , creditLogAmount   :: Integer
                                     , creditLogNonce    :: Integer
                                     , creditLogMemo     :: Text
                                     } deriving (Show, Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = over _head toLower . drop 9 } ''IssueCreditLog)

instance Eq IssueCreditLog where
    (==) (IssueCreditLog u1 c1 d1 a1 n1 _) (IssueCreditLog u2 c2 d2 a2 n2 _) =
            u1 == u2 && c1 == c2 && d1 == d2 && a1 == a2 && n1 == n2

data CreditRecord = CreditRecord { creditor              :: Address
                                 , debtor                :: Address
                                 , amount                :: Integer
                                 , memo                  :: Text
                                 , submitter             :: Address
                                 , nonce                 :: Integer
                                 , hash                  :: CreditHash
                                 , signature             :: Signature
                                 , ucac                  :: Address
                                 , settlementAmount      :: Maybe Integer
                                 , settlementCurrency    :: Maybe Text
                                 , settlementBlocknumber :: Maybe Integer
                                 } deriving (Show, Generic)
$(deriveJSON (defaultOptions { omitNothingFields = True }) ''CreditRecord)

data BilateralCreditRecord = BilateralCreditRecord { creditRecord :: CreditRecord
                                                   , creditorSignature :: Signature
                                                   , debtorSignature :: Signature
                                                   , txHash :: Maybe TransactionHash
                                                   } deriving (Show, Generic)
$(deriveJSON (defaultOptions { omitNothingFields = True }) ''BilateralCreditRecord)


data RejectRequest = RejectRequest { rejectRequestHash      :: Text
                                   , rejectRequestSignature :: Text
                                   }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 13 }) ''RejectRequest)

data NickRequest = NickRequest { nickRequestAddr      :: Address
                               , nickRequestNick      :: Text
                               , nickRequestSignature :: Text
                               } deriving Show
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 11 }) ''NickRequest)


data EmailRequest = EmailRequest { emailRequestAddr      :: Address
                                 , emailRequestEmail     :: EmailAddress
                                 , emailRequestSignature :: Text
                                 } deriving Show
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 12 }) ''EmailRequest)


data UserInfo = UserInfo { addr :: Address
                         , nick :: Maybe Text
                         } deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions { omitNothingFields = True } ''UserInfo)

data PushRequest = PushRequest { pushRequestChannelID :: Text
                               , pushRequestPlatform  :: Text
                               , pushRequestAddress   :: Address
                               , pushRequestSignature :: Text
                               }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 11 }) ''PushRequest)

data ProfilePhotoRequest = ProfilePhotoRequest { photoRequestImage :: Text
                                               , photoRequestSignature :: Text
                                               }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 12 }) ''ProfilePhotoRequest)


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
   toJSON Ios     = String "ios"
   toJSON Android = String "android"

-- This should probably be called 'Push Object' to match the Urban Airship docs
-- here: https://docs.urbanairship.com/api/ua/#push-object
data Notification = Notification { channelID :: Text
                                 , platform  :: DevicePlatform
                                 , message   :: Text
                                 , action    :: NotificationAction
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
        where deviceChannel Ios     = "ios_channel"
              deviceChannel Android = "android_channel"

-- A newtype wrapper is used for this 'Double' value which holds an Ethereum
-- price expressed in USD. This is necessary in order to have easy decoding
-- from the JSON response of the coinbase API.
data EthereumPrices = EthereumPrices { usd :: Double
                                     , jpy :: Double
                                     , krw :: Double
                                     , dkk :: Double
                                     , chf :: Double
                                     , cny :: Double
                                     , eur :: Double
                                     , aud :: Double
                                     , gbp :: Double
                                     , cad :: Double
                                     , nok :: Double
                                     , sek :: Double
                                     , nzd :: Double
                                     } deriving (Show, Generic)
$(deriveToJSON defaultOptions ''EthereumPrices)

instance Default EthereumPrices where
    def = EthereumPrices 420 45000 460000 2500 400 2600 340 550 300 540 3250 3450 575

instance FromJSON EthereumPrices where
        parseJSON (Object v) = do
            dataObject <- v .: "data"
            ratesObject <- dataObject .: "rates"
            usd <- read <$> ratesObject .: "USD"
            jpy <- read <$> ratesObject .: "JPY"
            krw <- read <$> ratesObject .: "KRW"
            dkk <- read <$> ratesObject .: "DKK"
            chf <- read <$> ratesObject .: "CHF"
            cny <- read <$> ratesObject .: "CNY"
            eur <- read <$> ratesObject .: "EUR"
            aud <- read <$> ratesObject .: "AUD"
            gbp <- read <$> ratesObject .: "GBP"
            cad <- read <$> ratesObject .: "CAD"
            nok <- read <$> ratesObject .: "NOK"
            sek <- read <$> ratesObject .: "SEK"
            nzd <- read <$> ratesObject .: "NZD"
            return $ EthereumPrices usd jpy krw dkk chf cny eur aud gbp cad nok sek nzd

data ServerConfig = ServerConfig { lndrUcacAddrs            :: B.Bimap Text Address
                                 , bindAddress              :: !Text
                                 , bindPort                 :: !Int
                                 , creditProtocolAddress    :: !Address
                                 , issueCreditEvent         :: !Text
                                 , scanStartBlock           :: !Integer
                                 , dbUser                   :: !Text
                                 , dbUserPassword           :: !Text
                                 , dbName                   :: !Text
                                 , dbHost                   :: !String
                                 , dbPort                   :: !Word16
                                 , gasPrice                 :: !Integer
                                 , ethereumPrices           :: !EthereumPrices
                                 , maxGas                   :: !Integer
                                 , latestBlockNumber        :: !Integer
                                 , urbanAirshipKey          :: !ByteString
                                 , urbanAirshipSecret       :: !ByteString
                                 , heartbeatInterval        :: !Int
                                 , awsPhotoBucket           :: !Text
                                 , awsAccessKeyId           :: !ByteString
                                 , awsSecretAccessKey       :: !ByteString
                                 , web3Url                  :: !String
                                 , executionPrivateKey      :: !Text
                                 , executionAddress         :: !Address
                                 }

-- 'ConfigResponse' contains all the server data that users have access to via
-- the /config endpoint. By and large, this endpoint is used by clients to
-- ensure their configuratoins match the server's.
data ConfigResponse = ConfigResponse { configResponseLndrAddresses :: M.Map Text Address
                                     , configResponseCreditProtocolAddress :: Address
                                     , configResponseGasPrice :: Integer
                                     , configResponseEthereumPrices :: EthereumPrices
                                     , configResponseWeekAgoBlock :: Integer
                                     } deriving (Show, Generic)
$(deriveToJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 14 }) ''ConfigResponse)

instance FromJSON ConfigResponse where
    parseJSON (Object v) = do
        addressesObject <- v .: "lndrAddresses"
        creditProtocolAddress <- v .: "creditProtocolAddress"
        gasPrice <- v .: "gasPrice"
        ethereumPricesObject <- v .: "ethereumPrices"
        weekAgoBlock <- v .: "weekAgoBlock"
        ethereumPrices <- EthereumPrices <$> ethereumPricesObject .: "usd"
                                         <*> ethereumPricesObject .: "jpy"
                                         <*> ethereumPricesObject .: "krw"
                                         <*> ethereumPricesObject .: "dkk"
                                         <*> ethereumPricesObject .: "chf"
                                         <*> ethereumPricesObject .: "cny"
                                         <*> ethereumPricesObject .: "eur"
                                         <*> ethereumPricesObject .: "aud"
                                         <*> ethereumPricesObject .: "gbp"
                                         <*> ethereumPricesObject .: "cad"
                                         <*> ethereumPricesObject .: "nok"
                                         <*> ethereumPricesObject .: "sek"
                                         <*> ethereumPricesObject .: "nzd"
        return $ ConfigResponse addressesObject creditProtocolAddress gasPrice
                                ethereumPrices weekAgoBlock

data SettlementsResponse = SettlementsResponse { unilateralSettlements :: [CreditRecord]
                                               , bilateralSettlements  :: [BilateralCreditRecord]
                                               } deriving Show
$(deriveJSON defaultOptions ''SettlementsResponse)

data ServerState = ServerState { dbConnectionPool :: Pool Connection
                               , serverConfig     :: TVar ServerConfig
                               , loggerSet        :: LoggerSet
                               }

data GasStationResponse = GasStationResponse { safeLow     :: Double
                                             , safeLowWait :: Double
                                             } deriving Show
$(deriveJSON defaultOptions ''GasStationResponse)

data VerifySettlementRequest = VerifySettlementRequest { verifySettlementRequestCreditHash :: CreditHash
                                                       , verifySettlementRequestTxHash :: TransactionHash
                                                       , verifySettlementRequestCreditorAddress :: Address
                                                       , verifySettlementRequestSignature :: Signature
                                                       }
$(deriveJSON (defaultOptions { fieldLabelModifier = over _head toLower . drop 23 }) ''VerifySettlementRequest)
