{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Dhall
import           GHC.Generics
import           Lndr.CLI.Config (Config(..))
import           Lndr.EthInterface hiding (getNonce)
import           Lndr.Types
import           Network.Ethereum.Util (hashPersonalMessage, ecsign)
import           Network.Ethereum.Web3
import qualified Network.HTTP.Simple as HTTP
import           System.Console.CmdArgs hiding (def)
import           System.Console.CmdArgs.Explicit(helpText, HelpFormat(..), modeEmpty)
import           System.Directory (doesFileExist)
import qualified Text.Pretty.Simple as Pr

data LndrCmd = Transactions
             | Pending
             | Lend { friend :: Text
                    , amount :: Integer
                    , memo :: Text
                    }
             | Borrow { friend :: Text
                      , amount :: Integer
                      , memo :: Text
                      }
             | Nick { nick :: Text }
             | GetNonce { friend :: Text }
             | Info
             deriving (Show, Data, Typeable)


programModes = modes [ Transactions &= help "list all transactions processed by Lndr UCAC"
                     , Pending &= help "list all pending transactions"
                     , Lend "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                            123
                            "default"
                            &= help "submit a unilateral transaction as a creditor"
                     , Borrow "0x198e13017d2333712bd942d8b028610b95c363da"
                              123
                              "default"
                              &= help "submit a unilateral transaction as a debtor"
                     , Nick "..." &= help "set a nickname for default user"
                     , GetNonce "0x198e13017d2333712bd942d8b028610b95c363da"
                     , Info &= help "prints config, nick, and friends"
                     ] &= help "Lend and borrow money" &= program "lndr" &= summary "lndr v0.1"


-- TODO put this in ReaderT to handle config vars loaded at runtime
-- (config var to implement: web address, userid)
runMode :: Config -> LndrCmd -> IO ()
runMode (Config url _ _ _) Transactions = do
    initReq <- HTTP.parseRequest $ LT.unpack url ++ "/transactions"
    resp <- HTTP.httpJSON initReq
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: [IssueCreditLog])
runMode (Config url _ _ _) Pending = do
    initReq <- HTTP.parseRequest $ LT.unpack url ++ "/pending"
    resp <- HTTP.httpJSON initReq
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: [PendingRecord])
runMode (Config url user sk _) (Lend friend amount memo) =
    submitCredit (LT.unpack url) (LT.toStrict sk) $ CreditRecord (LT.toStrict user) friend amount memo ""
runMode (Config url user sk _) (Borrow friend amount memo) =
    submitCredit (LT.unpack url) (LT.toStrict sk) $ CreditRecord friend (LT.toStrict user) amount memo ""
-- Friend-related Modes
runMode (Config url user sk _) (Nick nick) =
    let userAddr = textToAddress $ LT.toStrict user
    in print =<< setNick (LT.unpack url) (NickRequest userAddr nick "")
runMode (Config url user _ _) (GetNonce friend) =
    print =<< getNonce (LT.unpack url) (LT.toStrict user) friend


setNick :: String -> NickRequest -> IO Int
setNick url nickRequest = do
    initReq <- HTTP.parseRequest $ url ++ "/nick"
    let req = HTTP.setRequestBodyJSON nickRequest $
                HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req


getNonce :: String -> Text -> Text -> IO Integer
getNonce url addr1 addr2 = do
    req <- HTTP.parseRequest $ url ++ "/nonce/" ++ T.unpack addr1 ++ "/" ++ T.unpack addr2
    HTTP.getResponseBody <$> HTTP.httpJSON req


signCredit :: Text -> Integer -> CreditRecord Unsigned -> CreditRecord Signed
signCredit secretKey nonce r@(CreditRecord c d a m u) = r { signature = sig }
    where message = T.append "0x" . T.concat $
                        stripHexPrefix <$> [ ucacId
                                           , c
                                           , d
                                           , integerToHex a
                                           , integerToHex nonce
                                           ]
          hashedMessage = hashPersonalMessage message
          (Right sig) = ecsign hashedMessage secretKey


submitCredit :: String -> Text -> CreditRecord Unsigned -> IO ()
submitCredit url secretKey unsignedCredit@(CreditRecord creditor debtor _ _ _) = do
    nonce <- getNonce url debtor creditor
    initReq <- HTTP.parseRequest $ url ++ "/lend"
    let signedCredit = signCredit secretKey nonce unsignedCredit
    let req = HTTP.setRequestBodyJSON signedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpNoBody req
    Pr.pPrintNoColor (HTTP.getResponseStatusCode resp)


configPath = "~/.lndr.conf"


main :: IO ()
main = do mode <- cmdArgsRun $ cmdArgsMode programModes

          -- check for presence of a config file
          configExists <- doesFileExist configPath
          config <- if configExists
            then Dhall.input Dhall.auto "~/.lndr.conf"
            else putStrLn "using default config..." >> return def

          runMode config mode
