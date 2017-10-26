{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Network.Ethereum.Web3.Types hiding (Pending)
import qualified Network.HTTP.Simple as HTTP
import           System.Console.CmdArgs

data FiDCmd = Transactions
            | Pending
            | Lend { me :: Text
                   , friend :: Text
                   , amount :: Integer
                   }
            | Borrow { me :: Text
                     , friend :: Text
                     , amount :: Integer
                     }
            deriving (Show, Data, Typeable)

data SignedCredit = SignedCredit { creditor :: Text
                                 , debtor :: Text
                                 , amount :: Integer
                                 , signature :: Text
                                 }

data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Address
                                     , debtor :: Address
                                     , amount :: Integer
                                     } deriving Show

$(deriveJSON defaultOptions ''IssueCreditLog)
$(deriveJSON defaultOptions ''SignedCredit)

runMode :: FiDCmd -> IO ()
runMode Transactions = do resp <- HTTP.httpJSON "http://localhost:80/transactions"
                          print $ (HTTP.getResponseBody resp :: [IssueCreditLog])
runMode Pending = print "pending"
runMode Lend{} = print "lend"
runMode Borrow{} = print "borrow"

main :: IO ()
main = do mode <- cmdArgs (modes [ Transactions
                                 , Pending
                                 , Lend "" "" 0
                                 , Borrow "" "" 0
                                 ])
          runMode mode
