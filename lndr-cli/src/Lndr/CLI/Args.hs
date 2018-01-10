{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lndr.CLI.Args (
      LndrCmd(..)
    , programModes
    , runMode
    , userFromSK

    -- * nick-related requests
    , getNick
    , setNick
    , searchNick
    , takenNick

    -- * gas-related requests
    , getGasPrice
    , setGasPrice

    -- * friend-related requests
    , addFriend
    , getFriends
    , removeFriend

    -- * credit-related requests
    , checkPending
    , submitCredit
    , rejectCredit
    , getBalance
    , getTwoPartyBalance
    , getCounterparties
    , getTransactions
    , verifySettlement

    -- * notifications-related requests
    , registerChannel
    ) where

import           Data.Data
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Lndr.EthereumInterface hiding (getNonce)
import           Lndr.CLI.Config
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Util (hashPersonalMessage, ecsign, privateToAddress, hashText)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import qualified Network.HTTP.Simple as HTTP
import           System.Console.CmdArgs hiding (def)
import           System.Console.CmdArgs.Explicit(helpText, HelpFormat(..), modeEmpty)
import qualified Text.Pretty.Simple as Pr

data LndrCmd = Transactions
             | Pending
             | RejectPending
             | Lend { friend :: Text
                    , amount :: Integer
                    , memo :: Text
                    }
             | Borrow { friend :: Text
                      , amount :: Integer
                      , memo :: Text
                      }
             | Nick { nick :: Text }
             | SearchNick { nick :: Text }
             | GetNonce { friend :: Text }
             | AddFriend { friend :: Text }
             | RemoveFriend { friend :: Text }
             | GasPrice
             | SetGasPrice { price :: Integer }
             | Info
             | Unsubmitted
             deriving (Show, Data, Typeable)


programModes = modes [ Transactions &= help "list all transactions processed by Lndr UCAC"
                     , Pending &= help "list all pending transactions"
                     , RejectPending
                     , Lend "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                            123
                            "default"
                            &= help "submit a unilateral transaction as a creditor"
                     , Borrow "0x198e13017d2333712bd942d8b028610b95c363da"
                              123
                              "default"
                              &= help "submit a unilateral transaction as a debtor"
                     , Nick "aupiff" &= help "set a nickname for default user"
                     , SearchNick "aupiff" &= help "find address for a corresponding nickname"
                     , GetNonce "0x198e13017d2333712bd942d8b028610b95c363da"
                     , AddFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                     , RemoveFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                     , GasPrice
                     , SetGasPrice 2000000
                     , Unsubmitted &= help "prints txs that are in lndr db but not yet on the blockchain"
                     , Info &= help "prints config, nick, and friends"
                     ] &= help "Lend and borrow money" &= program "lndr" &= summary "lndr v0.1"


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
    httpCode <- submitCredit (LT.unpack url) (textToAddress $ LT.toStrict ucacAddr) (LT.toStrict sk) (CreditRecord (textToAddress $ userFromSK sk) (textToAddress friend) amount memo (textToAddress $ userFromSK sk) 0 "" "" Nothing Nothing Nothing)
    print httpCode

runMode (Config url sk ucacAddr) (Borrow friend amount memo) = do
    httpCode <- submitCredit (LT.unpack url) (textToAddress $ LT.toStrict ucacAddr) (LT.toStrict sk) (CreditRecord (textToAddress friend) (textToAddress $ userFromSK sk) amount memo (textToAddress $ userFromSK sk) 0 "" "" Nothing Nothing Nothing)

    print httpCode

-- Friend-related Modes
runMode (Config url sk _) (Nick nick) =
    let userAddr = textToAddress $ userFromSK sk
    in print =<< setNick (LT.unpack url) (NickRequest userAddr nick "")
runMode (Config url sk _) (SearchNick nick) =
    let userAddr = textToAddress $ userFromSK sk
    in print =<< searchNick (LT.unpack url) nick


runMode (Config url sk _) (AddFriend friend) =
    print =<< addFriend (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) (RemoveFriend friend) =
    print =<< removeFriend (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) GasPrice = print =<< getGasPrice (LT.unpack url)

runMode (Config url sk _) (SetGasPrice price) =
    print =<< setGasPrice (LT.unpack url) (textToAddress $ userFromSK sk) price


runMode (Config url sk _) (GetNonce friend) =
    print =<< getNonce (LT.unpack url) (textToAddress $ userFromSK sk) (textToAddress friend)

runMode (Config url sk _) Info =
    print =<< getInfo (LT.unpack url) (userFromSK sk)

runMode (Config url sk _) Unsubmitted =
    print =<< getUnsubmitted (LT.unpack url)

userFromSK = fromMaybe "" . privateToAddress . LT.toStrict


getUnsubmitted :: String -> IO [(Text, IssueCreditLog)]
getUnsubmitted url = do
    initReq <- HTTP.parseRequest $ url ++ "/unsubmitted"
    logs <- HTTP.getResponseBody <$> HTTP.httpJSON initReq
    return $ fmap (\x -> (hashCreditLog x, x)) logs


getTransactions :: String -> Address -> IO [IssueCreditLog]
getTransactions url address = do
    initReq <- HTTP.parseRequest $ url ++ "/transactions?user=" ++ show address
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


getSettlements :: IO ()
getSettlements = undefined


getCounterparties :: String -> Address -> IO [Address]
getCounterparties url address = do
    initReq <- HTTP.parseRequest $ url ++ "/counterparties/" ++ show address
    HTTP.getResponseBody <$> HTTP.httpJSON initReq


setGasPrice :: String -> Address -> Integer -> IO Int
setGasPrice url addr price = do
    initReq <- HTTP.parseRequest $ url ++ "/gas_price"
    let req = HTTP.setRequestBodyJSON price $
                HTTP.setRequestMethod "PUT" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req

getGasPrice :: String -> IO Integer
getGasPrice url = do
    req <- HTTP.parseRequest $ url ++ "/gas_price"
    resp <- HTTP.getResponseBody <$> HTTP.httpJSONEither req
    return $ case resp of
        Left a -> -1
        Right b -> b


setNick :: String -> NickRequest -> IO Int
setNick url nickRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/nick"
    let req = HTTP.setRequestBodyJSON nickRequest $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req

getNick :: String -> Address -> IO Text
getNick url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/nick/" ++ show userAddr
    resp <- HTTP.getResponseBody <$> HTTP.httpJSONEither req
    return $ case resp of
        Left a -> "nick not found"
        Right b -> b


searchNick :: String -> Text -> IO [NickInfo]
searchNick url nick = do
    req <- HTTP.parseRequest $ url ++ "/search_nick/" ++ T.unpack nick
    HTTP.getResponseBody <$> HTTP.httpJSON req


takenNick :: String -> Text -> IO Bool
takenNick url nick = do
    req <- HTTP.parseRequest $ url ++ "/taken_nick/" ++ T.unpack nick
    HTTP.getResponseBody <$> HTTP.httpJSON req


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


getFriends :: String -> Address -> IO [NickInfo]
getFriends url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/friends/" ++ show userAddr
    HTTP.getResponseBody <$> HTTP.httpJSON req


getBalance :: String -> Address -> IO Integer
getBalance url userAddr = do
    req <- HTTP.parseRequest $ url ++ "/balance/" ++ show userAddr
    HTTP.getResponseBody <$> HTTP.httpJSON req


getTwoPartyBalance :: String -> Address -> Address -> IO Integer
getTwoPartyBalance url userAddr counterPartyAddr = do
    let fullUrl = url ++ "/balance/" ++ show userAddr ++ "/" ++ show counterPartyAddr
    req <- HTTP.parseRequest fullUrl
    HTTP.getResponseBody <$> HTTP.httpJSON req


getInfo :: String -> Text -> IO (Address, Text, Integer, [NickInfo])
getInfo url userAddr = do
    nick <- getNick url address
    balance <- getBalance url address
    friends <- getFriends url address
    return (address, nick, balance, friends)
    where address = textToAddress userAddr


getNonce :: String -> Address -> Address -> IO Integer
getNonce url addr1 addr2 = do
    req <- HTTP.parseRequest $ url ++ "/nonce/" ++ T.unpack (Addr.toText addr1) ++ "/" ++ T.unpack (Addr.toText addr2)
    HTTP.getResponseBody <$> HTTP.httpJSON req


signCredit :: Text -> Address -> CreditRecord -> CreditRecord
signCredit secretKey ucacAddr r@(CreditRecord c d a m _ nonce _ _ _ _ _) = r { signature = sig , hash = message }
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
submitCredit :: String -> Address -> Text -> CreditRecord -> IO Int
submitCredit url ucacAddr secretKey unsignedCredit@(CreditRecord creditor debtor _ _ _ _ _ _ _ _ _) = do
    nonce <- getNonce url debtor creditor
    initReq <- if textToAddress (userFromSK (LT.fromStrict secretKey)) == creditor
                   then HTTP.parseRequest $ url ++ "/lend"
                   else HTTP.parseRequest $ url ++ "/borrow"
    let signedCredit = signCredit secretKey ucacAddr (unsignedCredit { nonce = nonce })
    let req = HTTP.setRequestBodyJSON signedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


rejectCredit :: String -> Text -> Text -> IO Int
rejectCredit url secretKey hash = do
    initReq <- HTTP.parseRequest $ url ++ "/reject"
    let (Right sig) = ecsign hash secretKey
        rejectRecord = RejectRecord sig hash
        req = HTTP.setRequestBodyJSON rejectRecord $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


verifySettlement :: String -> Text -> Text -> IO Int
verifySettlement url creditHash txHash = do
    let fullUrl = url ++ "/verify_settlement/" ++ T.unpack creditHash ++ "?txHash=" ++ T.unpack txHash
    req <- HTTP.setRequestMethod "POST" <$> HTTP.parseRequest fullUrl
    resp <- HTTP.httpNoBody req
    return $ HTTP.getResponseStatusCode resp


registerChannel :: String -> Address -> PushRequest -> IO Int
registerChannel url addr pushReq = do
    initReq <- HTTP.parseRequest $ url ++ "/register_push/" ++ show addr
    let req = HTTP.setRequestBodyJSON pushReq $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
