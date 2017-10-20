{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main
    ( main
    ) where

import Data.Text
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Api
import Network.Ethereum.Web3.Types
import System.Console.CmdArgs

data FiDCmd = Info
            | Init
            | Request {debtor :: String, amount :: Integer}
            | Send {creditor :: String, amount :: Integer}
            deriving (Show, Data, Typeable)

main :: IO ()
main = do mode <- cmdArgs (modes [Info, Init, Request "" 0, Send "" 0])
          runMode mode

runMode :: FiDCmd -> IO ()
runMode Info = print =<< runWeb3 allLogs
runMode _ = putStrLn "Not yet implemented"

-- fetch all logs
-- terminal equivalent: curl -X POST --data {"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock": "0x0"}],"id":73} localhost:8545
allLogs :: Provider a => Web3 a [Change]
allLogs = eth_getLogs (Filter Nothing Nothing (Just "0x0") Nothing)

-- fetch cp logs related to FiD UCAC

-- fetch cp logs related to a particular users use of the FiD UCAC

-- fetch cp logs related to a particular users use of the FiD UCAC
