{-# LANGUAGE OverloadedStrings     #-}

module Lndr.CLI.Actions (
      programModes
    , runMode
    , userFromSK

    -- * config-related requests
    , getConfig

    -- * nick-related requests
    , getNick
    , setNick
    , searchNick
    , takenNick

    -- * email-related requests
    , getEmail
    , setEmail
    , takenEmail

    -- * friend-related requests
    , addFriend
    , getFriends
    , getFriendRequests
    , removeFriend
    , setProfilePhoto

    -- * credit-related requests
    , checkPending
    , submitCredit
    , rejectCredit
    , getBalance
    , getTwoPartyBalance
    , getCounterparties
    , getTransactions
    , getPendingSettlements
    , getTxHash
    , getTxHashFail
    , scanBlockchain
    , consistencyCheck
    , verifySettlement
    , submitMultiSettlement
    , requestPayPal
    , retrievePayPalRequests
    , deletePayPalRequest

    -- * notifications-related requests
    , registerChannel
    , deleteChannel
    ) where

import           Control.Monad
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Base64          as B64
import           Data.Data
import           Data.Either                     (fromRight)
import           Data.List                       ((\\), find)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as LT
import qualified Database.PostgreSQL.Simple as DB
import           Lndr.CLI.Config
import           Lndr.Config
import qualified Lndr.Db                         as DB
import           Lndr.EthereumInterface          hiding (getNonce)
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util
import           Lndr.CLI.CmdLine
import           Network.Ethereum.Util           (ecsign, hashPersonalMessage,
                                                  hashText, privateToAddress)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address   as Addr
import           Network.Ethereum.Web3.Types     (Provider(..))
import qualified Network.HTTP.Simple             as HTTP
import           System.Directory
import           System.FilePath
import           Text.EmailAddress
import qualified Text.Pretty.Simple              as Pr

runMode :: Config -> LndrCmd -> IO ()
runMode (Config url sk _) Transactions = do
    logs <- getTransactions (LT.unpack url) (textToAddress $ userFromSK sk)
    Pr.pPrintNoColor logs

runMode (Config url sk _) Pending = do
    creditRecords <- checkPending (LT.unpack url) (textToAddress $ userFromSK sk)
    Pr.pPrintNoColor creditRecords

runMode (Config url sk _) RejectPending = do
    req <- HTTP.parseRequest $ LT.unpack url ++ "/pending/" ++ T.unpack (userFromSK sk)
    records <- HTTP.getResponseBody <$> HTTP.httpJSON req :: IO [CreditRecord]
    Pr.pPrintNoColor records
    index <- (read :: String -> Int) <$> getLine
    httpCode <- rejectCredit (LT.unpack url) (LT.toStrict sk) (hash $ records !! index)
    print httpCode

runMode (Config url sk ucacAddr) (Lend friend amount memo) = do
    httpCode <- submitCredit (LT.unpack url) (LT.toStrict sk) (CreditRecord (textToAddress $ userFromSK sk) (textToAddress friend) amount memo (textToAddress $ userFromSK sk) 0 "" "" (textToAddress $ LT.toStrict ucacAddr) Nothing Nothing Nothing)
    print httpCode

runMode (Config url sk ucacAddr) (Borrow friend amount memo) = do
    httpCode <- submitCredit (LT.unpack url) (LT.toStrict sk) (CreditRecord (textToAddress friend) (textToAddress $ userFromSK sk) amount memo (textToAddress $ userFromSK sk) 0 "" "" (textToAddress $ LT.toStrict ucacAddr) Nothing Nothing Nothing)

    print httpCode

-- Friend-related Modes
runMode (Config url sk _) (Nick nick) =
    let userAddr = textToAddress $ userFromSK sk
    in print =<< setNick (LT.unpack url) (LT.toStrict sk) (NickRequest userAddr nick "")
runMode (Config url sk _) (SearchNick nick) =
    let userAddr = textToAddress $ userFromSK sk
    in print =<< searchNick (LT.unpack url) nick

runMode (Config url sk _) (AddFriend friend) =
    print =<< addFriend (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) (RemoveFriend friend) =
    print =<< removeFriend (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) (SetPhoto photoPath) =
    print =<< setProfilePhoto (LT.unpack url) (LT.toStrict sk) photoPath

runMode (Config url sk _) (GetNonce friend) =
    print =<< getNonce (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) Info =
    print =<< getInfo (LT.unpack url) (userFromSK sk)

runMode (Config url sk _) PendingSettlements =
    print =<< getPendingSettlements (LT.unpack url) (textToAddress $ userFromSK sk)

runMode (Config url sk _) LndrConfig =
    print =<< getConfig (LT.unpack url)

runMode (Config url sk _) ScanBlockchain = do
    logs <- scanBlockchain
    Pr.pPrintNoColor logs

runMode (Config url sk _) ConsistencyCheck = do
    checkOutput <- consistencyCheck
    Pr.pPrintNoColor checkOutput


userFromSK = fromMaybe "" . privateToAddress . LT.toStrict

-- TODO all cmdline actions should be put into `Reader Config` monad
-- OR   we can use the servant autogen'd client code

getTransactions :: String -> Address -> IO [IssueCreditLog]
getTransactions url address = do
    initReq <- HTTP.parseRequest $ url ++ "/transactions?user=" ++ show address
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


getPendingSettlements :: String -> Address -> IO SettlementsResponse
getPendingSettlements url address = do
    initReq <- HTTP.parseRequest $ url ++ "/pending_settlements/" ++ show address
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


getCounterparties :: String -> Address -> IO [Address]
getCounterparties url address = do
    initReq <- HTTP.parseRequest $ url ++ "/counterparties/" ++ show address
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


setNick :: String -> Text -> NickRequest -> IO Int
setNick url sk nickRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/nick"
    let Right signature = generateSignature nickRequest sk
        req = HTTP.setRequestBodyJSON (nickRequest { nickRequestSignature = signature }) $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req

getNick :: String -> Address -> IO Text
getNick url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/nick/" ++ show userAddr
    resp <- HTTP.getResponseBody <$> HTTP.httpJSONEither req
    return $ case resp of
        Left a  -> "nick not found"
        Right b -> b


searchNick :: String -> Text -> IO [UserInfo]
searchNick url nick = do
    req <- HTTP.parseRequest $ url ++ "/search_nick/" ++ T.unpack nick
    HTTP.getResponseBody <$> HTTP.httpJSON req


setEmail :: String -> Text -> EmailRequest -> IO Int
setEmail url sk emailRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/email"
    let Right signature = generateSignature emailRequest sk
        req = HTTP.setRequestBodyJSON (emailRequest { emailRequestSignature = signature }) $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


getEmail :: String -> Address -> IO EmailAddress
getEmail url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/email/" ++ show userAddr
    HTTP.getResponseBody <$> HTTP.httpJSON req


takenEmail :: String -> Text -> IO Bool
takenEmail url email = do
    req <- HTTP.parseRequest $ url ++ "/user?email=" ++ T.unpack email
    httpCode <- HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    return $ httpCode == 200


takenNick :: String -> Text -> IO Bool
takenNick url nick = do
    req <- HTTP.parseRequest $ url ++ "/user?nick=" ++ T.unpack nick
    httpCode <- HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    return $ httpCode == 200


addFriend :: String -> Address -> Address -> IO Int
addFriend url userAddr addr = do
    initReq <- HTTP.parseRequest $ url ++ "/add_friends/" ++ show userAddr
    let req = HTTP.setRequestBodyJSON [addr] $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


removeFriend :: String -> Address -> Address -> IO Int
removeFriend url userAddr addr = do
    initReq <- HTTP.parseRequest $ url ++ "/remove_friends/" ++ show userAddr
    let req = HTTP.setRequestBodyJSON [addr] $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


getFriends :: String -> Address -> IO [UserInfo]
getFriends url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/friends/" ++ show userAddr
    HTTP.getResponseBody <$> HTTP.httpJSON req


getFriendRequests :: String -> Address -> IO [UserInfo]
getFriendRequests url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/friend_requests/" ++ show userAddr
    HTTP.getResponseBody <$> HTTP.httpJSON req


getBalance :: String -> Address -> String -> IO Integer
getBalance url userAddr currency = do
    req <- HTTP.parseRequest $ url ++ "/balance/" ++ show userAddr ++ "?currency=" ++ currency
    HTTP.getResponseBody <$> HTTP.httpJSON req


getTwoPartyBalance :: String -> Address -> Address -> String -> IO Integer
getTwoPartyBalance url userAddr counterPartyAddr currency = do
    let fullUrl = url ++ "/balance/" ++ show userAddr ++ "/"
                      ++ show counterPartyAddr ++ "?currency=" ++ currency
    req <- HTTP.parseRequest fullUrl
    HTTP.getResponseBody <$> HTTP.httpJSON req


getInfo :: String -> Text -> IO (Address, Text, Integer, [UserInfo])
getInfo url userAddr = do
    nick <- getNick url address
    balance <- getBalance url address "USD"
    friends <- getFriends url address
    return (address, nick, balance, friends)
    where address = textToAddress userAddr


getNonce :: String -> Address -> Address -> IO Integer
getNonce url addr1 addr2 = do
    req <- HTTP.parseRequest $ url ++ "/nonce/" ++ T.unpack (Addr.toText addr1) ++ "/" ++ T.unpack (Addr.toText addr2)
    HTTP.getResponseBody <$> HTTP.httpJSON req


getTxHash :: String -> Text -> IO Text
getTxHash url creditHash = do
    req <- HTTP.parseRequest $ url ++ "/tx_hash/" ++ T.unpack creditHash
    HTTP.getResponseBody <$> HTTP.httpJSON req


-- TODO how should I handle HTTP errors nicely in these tests?
getTxHashFail :: String -> Text -> IO Int
getTxHashFail url creditHash = do
    req <- HTTP.parseRequest $ url ++ "/tx_hash/" ++ T.unpack creditHash
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


signCredit :: Text -> CreditRecord -> CreditRecord
signCredit secretKey r@(CreditRecord c d a m _ nonce _ _ ucacAddr _ _ _) = r { signature = sig , hash = message }
    where message = hashText . T.concat $
                        stripHexPrefix <$> [ Addr.toText ucacAddr
                                           , Addr.toText c
                                           , Addr.toText d
                                           , integerToHex a
                                           , integerToHex nonce
                                           ]
          hashedMessage = hashPersonalMessage message
          (Right sig) = ecsign hashedMessage secretKey


checkPending :: String -> Address -> IO [CreditRecord]
checkPending url userAddress = do
    initReq <- HTTP.parseRequest $ url ++ "/pending/" ++ show userAddress
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


-- TODO Don't take a credit record
submitCredit :: String -> Text -> CreditRecord -> IO Int
submitCredit url secretKey unsignedCredit@(CreditRecord creditor debtor _ _ _ _ _ _ ucacAddr _ _ _) = do
    nonce <- getNonce url debtor creditor
    initReq <- if textToAddress (userFromSK (LT.fromStrict secretKey)) == creditor
                   then HTTP.parseRequest $ url ++ "/lend"
                   else HTTP.parseRequest $ url ++ "/borrow"
    let signedCredit = signCredit secretKey (unsignedCredit { nonce = nonce })
        req = HTTP.setRequestBodyJSON signedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


submitMultiSettlement :: String -> Text -> [CreditRecord] -> IO Int
submitMultiSettlement url secretKey transactions = do
    startNonce <- getNonce url (debtor $ head transactions) (creditor $ head transactions)
    initReq <- HTTP.parseRequest $ url ++ "/multi_settlement"
    let signedTransactions = fmap (\tx -> signCredit secretKey tx ) transactions
        req = HTTP.setRequestBodyJSON signedTransactions $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


rejectCredit :: String -> Text -> Text -> IO Int
rejectCredit url secretKey hash = do
    initReq <- HTTP.parseRequest $ url ++ "/reject"
    let (Right sig) = ecsign hash secretKey
        rejectRecord = RejectRequest hash sig
        req = HTTP.setRequestBodyJSON rejectRecord $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


getConfig :: String -> IO ConfigResponse
getConfig url = do
    req <- HTTP.parseRequest $ url ++ "/config"
    HTTP.getResponseBody <$> HTTP.httpJSON req


verifySettlement :: String -> Text -> Text -> Text -> IO Int
verifySettlement url creditHash txHash privateKey = do
    let address = textToAddress . fromMaybe "" . privateToAddress $ privateKey
        verifyRequest' = VerifySettlementRequest creditHash txHash address ""
    initReq <- HTTP.parseRequest $ url ++ "/verify_settlement"
    let Right signature = generateSignature verifyRequest' privateKey
        verifyRequest = verifyRequest' { verifySettlementRequestSignature = signature }
        req = HTTP.setRequestBodyJSON verifyRequest $
            HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


registerChannel :: String -> Text -> PushRequest -> IO Int
registerChannel url privateKey pushReq = do
    initReq <- HTTP.parseRequest $ url ++ "/register_push"
    let Right signature = generateSignature pushReq privateKey
        req = HTTP.setRequestBodyJSON (pushReq { pushRequestSignature = signature }) $
                    HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


deleteChannel :: String -> Text -> PushRequest -> IO Int
deleteChannel url privateKey delPushReq = do
    initReq <- HTTP.parseRequest $ url ++ "/unregister_push"
    let Right signature = generateSignature delPushReq privateKey
        req = HTTP.setRequestBodyJSON (delPushReq { pushRequestSignature = signature }) $
                    HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


setProfilePhoto :: String -> Text -> FilePath -> IO Int
setProfilePhoto url privateKey photoPath = do
    imageData <- T.decodeUtf8 . B64.encode <$> B.readFile photoPath
    initReq <- HTTP.parseRequest $ url ++ "/profile_photo"
    let photoRequest = ProfilePhotoRequest imageData ""
        (Right sig) = generateSignature photoRequest privateKey
        signedPhotoRequest = photoRequest { photoRequestSignature = sig }
        req = HTTP.setRequestBodyJSON signedPhotoRequest $
                    HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


scanBlockchain :: IO (Either Web3Error [IssueCreditLog])
scanBlockchain = do
    home <- getHomeDirectory
    config <- loadConfig $  home </> "lndr-server.config"
    runWeb3' (HttpProvider (web3Url config)) $
        join <$> sequence [ lndrLogs config "AUD" Nothing Nothing
                          , lndrLogs config "CAD" Nothing Nothing
                          , lndrLogs config "CHF" Nothing Nothing
                          , lndrLogs config "CNY" Nothing Nothing
                          , lndrLogs config "DKK" Nothing Nothing
                          , lndrLogs config "EUR" Nothing Nothing
                          , lndrLogs config "GBP" Nothing Nothing
                          , lndrLogs config "HKD" Nothing Nothing
                          , lndrLogs config "IDR" Nothing Nothing
                          , lndrLogs config "ILS" Nothing Nothing
                          , lndrLogs config "INR" Nothing Nothing
                          , lndrLogs config "JPY" Nothing Nothing
                          , lndrLogs config "KRW" Nothing Nothing
                          , lndrLogs config "MYR" Nothing Nothing
                          , lndrLogs config "NOK" Nothing Nothing
                          , lndrLogs config "NZD" Nothing Nothing
                          , lndrLogs config "RUB" Nothing Nothing
                          , lndrLogs config "SEK" Nothing Nothing
                          , lndrLogs config "SGD" Nothing Nothing
                          , lndrLogs config "THB" Nothing Nothing
                          , lndrLogs config "TRY" Nothing Nothing
                          , lndrLogs config "USD" Nothing Nothing
                          , lndrLogs config "VND" Nothing Nothing ]


scanDB :: IO [IssueCreditLog]
scanDB = do
   home <- getHomeDirectory
   config <- loadConfig $  home </> "lndr-server.config"
   let dbConfig = DB.defaultConnectInfo {
                     DB.connectHost = dbHost config
                   , DB.connectPort = dbPort config
                   , DB.connectUser = T.unpack $ dbUser config
                   , DB.connectPassword = T.unpack $ dbUserPassword config
                   , DB.connectDatabase = T.unpack $ dbName config
                   }
   connection <- DB.connect dbConfig
   DB.allCredits connection


consistencyCheck :: IO (Int, Int, [(Text, IssueCreditLog)], [(Text, IssueCreditLog)])
consistencyCheck = do
   blockchainCredits <- fromRight [] <$> scanBlockchain
   dbCredits <- scanDB
   pure ( length dbCredits
        , length blockchainCredits
        , hashPair <$> dbCredits \\ blockchainCredits
        , hashPair <$> blockchainCredits \\ dbCredits)
    where hashPair creditLog = (hashCreditLog creditLog, creditLog)


requestPayPal :: String -> Text -> PayPalRequest -> IO Int
requestPayPal url sk paypalRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/request_paypal"
    let Right signature = generateSignature paypalRequest sk
        req = HTTP.setRequestBodyJSON (paypalRequest { paypalRequestSignature = signature }) $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


deletePayPalRequest :: String -> Text -> PayPalRequest -> IO Int
deletePayPalRequest url sk paypalRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/remove_paypal_request"
    let Right signature = generateSignature paypalRequest sk
        req = HTTP.setRequestBodyJSON (paypalRequest { paypalRequestSignature = signature }) $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


retrievePayPalRequests :: String -> Address -> IO [PayPalRequestPair]
retrievePayPalRequests url addr = do
    req <- HTTP.parseRequest $ url ++ "/request_paypal/" ++ T.unpack (Addr.toText addr)
    HTTP.getResponseBody <$> HTTP.httpJSON req
