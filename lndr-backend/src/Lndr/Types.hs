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
    , PayPalRequest(..)
    , PayPalRequestPair(..)

    -- * push notifications-related types
    , PushRequest(..)
    , Notification(..)
    , NotificationAction(..)
    , DevicePlatform(..)

    -- * identity verification types
    , IdentityVerificationRequest(..)
    , IdentityVerificationInfo(..)
    , IdentityAddress(..)
    , IdentityDocument(..)
    , IdentityStatusReview(..)
    , IdentityVerificationStatus(..)
    , VerificationStatusRequest(..)
    , VerificationStatusEntry(..)
    , IdentityResponseInfo(..)
    , IdentityVerificationResponse(..)
    , RequiredIdentityDocuments(..)
    , IdentityDocumentType(..)

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
                                 , settlementCurrency    :: Maybe Text
                                 , settlementAmount      :: Maybe Integer
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
                        | NewFriendRequest
                        | RequestPayPal
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

-- This should match the field names used in the Notifications API
-- here: https://github.com/blockmason/lndr-notifications
data Notification = Notification { channelID :: Text
                                 , platform  :: DevicePlatform
                                 , user   :: Maybe Text
                                 , notificationType  :: NotificationAction
                                 } deriving Show

instance ToJSON Notification where
    toJSON (Notification channelID platform user notificationType) =
        object [ "channelID" .= channelID
               , "platform" .= platform
               , "user" .= user
               , "notificationType" .= notificationType ]

-- A newtype wrapper is used for this 'Double' value which holds an Ethereum
-- price expressed in USD. This is necessary in order to have easy decoding
-- from the JSON response of the coinbase API.
data EthereumPrices = EthereumPrices { aud :: Double --
                                     , cad :: Double
                                     , chf :: Double
                                     , cny :: Double
                                     , dkk :: Double
                                     , eur :: Double
                                     , gbp :: Double
                                     , hkd :: Double
                                     , idr :: Double
                                     , ils :: Double
                                     , inr :: Double
                                     , jpy :: Double
                                     , krw :: Double
                                     , myr :: Double
                                     , nok :: Double
                                     , nzd :: Double
                                     , pln :: Double
                                     , rub :: Double
                                     , sek :: Double
                                     , sgd :: Double
                                     , thb :: Double
                                     , try :: Double
                                     , usd :: Double
                                     , vnd :: Double
                                     } deriving (Show, Generic)
$(deriveToJSON defaultOptions ''EthereumPrices)

instance Default EthereumPrices where
    def = EthereumPrices 675 650 500 3400 3200 428 380 3900 7265 1830 34475 55620 564600 2030 4100 735 1890 31665 4410 680 17000 2440 500 11625

instance FromJSON EthereumPrices where
        parseJSON (Object v) = do
            dataObject <- v .: "data"
            ratesObject <- dataObject .: "rates"
            aud <- read <$> ratesObject .: "AUD"
            cad <- read <$> ratesObject .: "CAD"
            chf <- read <$> ratesObject .: "CHF"
            cny <- read <$> ratesObject .: "CNY"
            dkk <- read <$> ratesObject .: "DKK"
            eur <- read <$> ratesObject .: "EUR"
            gbp <- read <$> ratesObject .: "GBP"
            hkd <- read <$> ratesObject .: "HKD"
            idr <- read <$> ratesObject .: "IDR"
            ils <- read <$> ratesObject .: "ILS"
            inr <- read <$> ratesObject .: "INR"
            jpy <- read <$> ratesObject .: "JPY"
            krw <- read <$> ratesObject .: "KRW"
            myr <- read <$> ratesObject .: "MYR"
            nok <- read <$> ratesObject .: "NOK"
            nzd <- read <$> ratesObject .: "NZD"
            pln <- read <$> ratesObject .: "PLN"
            rub <- read <$> ratesObject .: "RUB"
            sek <- read <$> ratesObject .: "SEK"
            sgd <- read <$> ratesObject .: "SGD"
            thb <- read <$> ratesObject .: "THB"
            try <- read <$> ratesObject .: "TRY"
            usd <- read <$> ratesObject .: "USD"
            vnd <- read <$> ratesObject .: "VND"
            return $ EthereumPrices aud cad chf cny dkk eur gbp hkd idr ils inr jpy krw myr nok nzd pln rub sek sgd thb try usd vnd

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
                                 , heartbeatInterval        :: !Int
                                 , awsPhotoBucket           :: !Text
                                 , awsAccessKeyId           :: !ByteString
                                 , awsSecretAccessKey       :: !ByteString
                                 , notificationsApiUrl      :: !String
                                 , notificationsApiKey      :: !ByteString
                                 , sumsubApiUrl      :: !String
                                 , sumsubApiKey      :: !String
                                 , web3Url                  :: !String
                                 , executionPrivateKey      :: !Text
                                 , executionAddress         :: !Address
                                 , executionNonce           :: !Integer
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
        ethereumPrices <- EthereumPrices <$> ethereumPricesObject .: "aud"
                                         <*> ethereumPricesObject .: "cad"
                                         <*> ethereumPricesObject .: "chf"
                                         <*> ethereumPricesObject .: "cny"
                                         <*> ethereumPricesObject .: "dkk"
                                         <*> ethereumPricesObject .: "eur"
                                         <*> ethereumPricesObject .: "gbp"
                                         <*> ethereumPricesObject .: "hkd"
                                         <*> ethereumPricesObject .: "idr"
                                         <*> ethereumPricesObject .: "ils"
                                         <*> ethereumPricesObject .: "inr"
                                         <*> ethereumPricesObject .: "jpy"
                                         <*> ethereumPricesObject .: "krw"
                                         <*> ethereumPricesObject .: "myr"
                                         <*> ethereumPricesObject .: "nok"
                                         <*> ethereumPricesObject .: "nzd"
                                         <*> ethereumPricesObject .: "pln"
                                         <*> ethereumPricesObject .: "rub"
                                         <*> ethereumPricesObject .: "sek"
                                         <*> ethereumPricesObject .: "sgd"
                                         <*> ethereumPricesObject .: "thb"
                                         <*> ethereumPricesObject .: "try"
                                         <*> ethereumPricesObject .: "usd"
                                         <*> ethereumPricesObject .: "vnd"
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

data PayPalRequest = PayPalRequest { friend :: Address
                                   , requestor :: Address
                                   , paypalRequestSignature :: Text
                                   } deriving Show
$(deriveJSON defaultOptions ''PayPalRequest)

data PayPalRequestPair = PayPalRequestPair { friend :: UserInfo
                                           , requestor :: UserInfo
                                           } deriving Show
$(deriveJSON defaultOptions ''PayPalRequestPair)

data IdentityAddress = IdentityAddress { street :: Text	-- String	No	3-letter country code
                                       , flatNumber :: Text	-- String	No	Flat or apartment number
                                       , town :: Text	-- String	No	Town or city name
                                       , state :: Text	-- String	No	State name if applicable
                                       , postCode :: Text	-- String	No	Postal code
                                       , country :: Text	-- String	No	Street name
                                       } deriving Show
$(deriveJSON defaultOptions ''IdentityAddress)

data IdentityDocument = IdentityDocument { idDocType :: Text -- String	Yes	See supported document types below
                                         , idDocSubType :: Text -- String	No	FRONT_SIDE, BACK_SIDE or null
                                         , country :: Text -- String	Yes	3-letter country code (Wikipedia)
                                         , firstName :: Text -- String	No	First name
                                         , middleName :: Text -- String	No	Middle name
                                         , lastName :: Text -- String	No	Last name
                                        --  , issuedDate :: Text -- String	No	Issued date (yyyy-MM-dd)
                                        --  , validUntil :: Text -- String	No	Valid until date (yyyy-MM-dd)
                                         } deriving Show
$(deriveJSON defaultOptions ''IdentityDocument)

data IdentityDocumentType = IdentityDocumentType { idDocSetType :: Text
                                                 , types :: [Text]
                                                 , subTypes :: Maybe [Text]
                                                 } deriving Show
$(deriveJSON defaultOptions ''IdentityDocumentType)

data RequiredIdentityDocuments = RequiredIdentityDocuments { country :: Text
                                                           , docSets :: [IdentityDocumentType]
                                                           } deriving Show
$(deriveJSON defaultOptions ''RequiredIdentityDocuments)

data IdentityVerificationInfo = IdentityVerificationInfo { country :: Text
                                           , firstName :: Text
                                           , middleName :: Text
                                           , lastName :: Text
                                           , phone :: Text
                                           , dob :: Text
                                           , nationality :: Text
                                           , addresses :: [IdentityAddress]
                                           , idDocs :: [IdentityDocument]
                                           , requiredIdDocs :: RequiredIdentityDocuments
                                           } deriving Show
$(deriveJSON defaultOptions ''IdentityVerificationInfo)

data IdentityVerificationRequest = IdentityVerificationRequest { email :: EmailAddress
                                           , externalUserId :: Address
                                           , info :: IdentityVerificationInfo
                                           , identitySignature :: Signature
                                           } deriving Show
$(deriveJSON defaultOptions ''IdentityVerificationRequest)

data IdentityResponseInfo = IdentityResponseInfo { firstName :: Text --"Serge",
                                             , middleName :: Text --"Sergeevich",
                                             , lastName :: Text --"Sergeew",
                                             , dob :: Text --"2000-03-04",
                                             , placeOfBirth :: Text --"Saint-Petersburg",
                                             , country :: Text --"RUS",
                                             , phone :: Text --"+7-911-2081223"
                                             } deriving Show
$(deriveJSON defaultOptions ''IdentityResponseInfo)

data IdentityVerificationResponse = IdentityVerificationResponse { id :: Text--"596eb3c93a0eb985b8ade34d",
                                            , createdAt :: Text--"2017-07-19 03:20:09",
                                            , inspectionId :: Text--"596eb3c83a0eb985b8ade349",
                                            , jobId :: Text--"a8f77946-14ff-4398-aa23-a1027e16f627",
                                            , info :: IdentityResponseInfo
                                            , email :: EmailAddress
                                            }
$(deriveJSON defaultOptions ''IdentityVerificationResponse)

data IdentityStatusReview = IdentityStatusReview { reviewAnswer :: Text
                                            , clientComment :: Text
                                            , moderationComment :: Maybe Text
                                            , rejectLabels :: Maybe [String]
                                            , reviewRejectType :: Maybe Text
                                            } deriving Show
$(deriveJSON defaultOptions ''IdentityStatusReview)

data IdentityVerificationStatus = IdentityVerificationStatus { applicantId :: Text
                                            , inspectionId :: Text
                                            , correlationId :: Text
                                            , externalUserId :: Address
                                            , success :: Bool
                                            , details :: Maybe Text
                                            , _type :: Text
                                            , review :: IdentityStatusReview
                                            } deriving Show
$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "_type" then "type" else x} ''IdentityVerificationStatus)

data VerificationStatusRequest = VerificationStatusRequest { user :: Address
                                           , verificationStatusSignature :: Signature
                                           } deriving Show
$(deriveJSON defaultOptions ''VerificationStatusRequest)

data VerificationStatusEntry = VerificationStatusEntry { user :: Address
                                           , sumsubId :: Text
                                           , status :: Text
                                           } deriving Show
$(deriveJSON defaultOptions ''VerificationStatusEntry)
