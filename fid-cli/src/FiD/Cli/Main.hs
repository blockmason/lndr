{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main
    ( main
    ) where

import Data.Text
import qualified Data.Text.Lazy as LT
import Dhall hiding (Text)
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Api
import Network.Ethereum.Web3.Types
import System.Console.CmdArgs hiding (auto)

-- TODO can I get rid of this redundant configFile param via Cmd Product Type?
data FiDCmd = Info    {config :: Text, scope :: Text}
            | Request {config :: Text, debtor :: Text, amount :: Integer}
            | Send    {config :: Text, creditor :: Text, amount :: Integer}
            deriving (Show, Data, Typeable)

--  validate these to make sure they're all valid
--  should they all be integers? why not?
--  is there aleardy an efficient uint256 type in haskell?
data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Text
                                     , debtor :: Text
                                     , amount :: Text
                                     } deriving Show

data FiDConfig = FiDConfig { fidAddress :: Text
                           , cpAddress :: Text
                           , userAddress :: Text
                           } deriving (Show, Generic)

instance Interpret FiDConfig

main :: IO ()
main = do mode <- cmdArgs (modes [Info "" "all", Request "" "" 0, Send "" "" 0])
          let configFilePath = config mode
          x <- input auto $ LT.fromStrict configFilePath
          print (x :: FiDConfig)
          runMode mode

runMode :: FiDCmd -> IO ()
runMode (Info _ "fid") = print =<< runWeb3 fidLogs
runMode (Info _ "all") = print =<< runWeb3 allLogs
runMode _ = putStrLn "Not yet implemented"

-- fetch all logs
-- terminal equivalent: curl -X POST --data {"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock": "0x0"}],"id":73} localhost:8545
allLogs :: Provider a => Web3 a [Change]
allLogs = eth_getLogs (Filter Nothing Nothing (Just "0x0") Nothing)

-- fetch cp logs related to FiD UCAC
-- verify that these are proper logs
fidLogs :: Provider a => Web3 a [IssueCreditLog]
fidLogs = fmap interpretUcacLog <$>
    eth_getLogs (Filter (Just "0xd5ec73eac35fc9dd6c3f440bce314779fed09f60")
                        Nothing
                        (Just "0x0")
                        Nothing)

interpretUcacLog :: Change -> IssueCreditLog
interpretUcacLog change = IssueCreditLog (changeAddress change)
                                         ""
                                         ""
                                         (changeData change)

-- fetch cp logs related to a particular users use of the FiD UCAC

-- fetch cp logs related to a particular users use of the FiD UCAC
