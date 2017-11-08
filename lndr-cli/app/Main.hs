{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import           System.Console.CmdArgs
import qualified Text.Pretty.Simple as Pr
import           EthInterface

data FiDCmd = Transactions { url :: String }
            | Pending { url :: String }
            | Lend { me :: Text
                   , friend :: Text
                   , amount :: Integer
                   , memo :: Text
                   , url :: String
                   }
            | Borrow { me :: Text
                     , friend :: Text
                     , amount :: Integer
                     , memo :: Text
                     , url :: String
                     }
            deriving (Show, Data, Typeable)

-- TODO put this in ReaderT to handle config vars loaded at runtime
-- (config var to implement: web address, userid)
runMode :: FiDCmd -> IO ()
runMode (Transactions url) = do
    initReq <- HTTP.parseRequest $ url ++ "/transactions"
    resp <- HTTP.httpJSON initReq
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: [IssueCreditLog])
runMode (Pending url) = do
    initReq <- HTTP.parseRequest $ url ++ "/pending"
    resp <- HTTP.httpJSON initReq
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: [(Text, CreditRecord Signed)])
runMode (Lend user friend amount memo url) = submitCredit url $ CreditRecord user friend amount memo user
runMode (Borrow user friend amount memo url) = submitCredit url $ CreditRecord friend user amount memo user


submitCredit :: String -> CreditRecord Unsigned -> IO ()
submitCredit url unsignedCredit = do
    initReq <- HTTP.parseRequest $ url ++ "/lend"
    let req = HTTP.setRequestBodyJSON unsignedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpJSON req
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: ())

programModes = modes [ Transactions defaultServerUrl &= help "list all transactions processed by FiD UCAC"
                     , Pending defaultServerUrl &= help "list all pending transactions"
                     , Lend "0x198e13017d2333712bd942d8b028610b95c363da"
                            "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                            123
                            "default"
                            defaultServerUrl &= help "submit a unilateral transaction as a creditor"
                     , Borrow "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                              "0x198e13017d2333712bd942d8b028610b95c363da"
                              123
                              "default"
                              defaultServerUrl &= help "submit a unilateral transaction as a debtor"
                     ] &= help "Lend and borrow money" &= program "fiddy" &= summary "fiddy v0.1"
    where defaultServerUrl = "http://34.202.214.156:80"

main :: IO ()
main = do mode <- cmdArgsRun $ cmdArgsMode programModes
          runMode mode
