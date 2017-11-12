{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Dhall
import           GHC.Generics
import           Lndr.CLI.Config (Config(..))
import           Lndr.EthInterface
import           Lndr.Types
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
             deriving (Show, Data, Typeable)


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
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: [(Text, CreditRecord Signed)])
runMode (Config url user _ _) (Lend friend amount memo) =
    submitCredit (LT.unpack url) $ CreditRecord (LT.toStrict user) friend amount memo ""
runMode (Config url user _ _) (Borrow friend amount memo) =
    submitCredit (LT.unpack url) $ CreditRecord friend (LT.toStrict user) amount memo ""

submitCredit :: String -> CreditRecord Unsigned -> IO ()
submitCredit url unsignedCredit = do
    initReq <- HTTP.parseRequest $ url ++ "/lend"
    let req = HTTP.setRequestBodyJSON unsignedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpJSON req
    Pr.pPrintNoColor (HTTP.getResponseBody resp :: ())

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
                     ] &= help "Lend and borrow money" &= program "lndr" &= summary "lndr v0.1"


configPath = "~/.lndr.conf"


main :: IO ()
main = do mode <- cmdArgsRun $ cmdArgsMode programModes

          -- check for presence of a config file
          configExists <- doesFileExist configPath
          config <- if configExists
            then Dhall.input Dhall.auto "~/.lndr.conf"
            else putStrLn "using default config..." >> return def

          runMode config mode
