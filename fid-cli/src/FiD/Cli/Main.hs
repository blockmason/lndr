{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main
    ( main
    ) where

import           Data.Aeson
import           Data.Either.Combinators (rightToMaybe, fromRight)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Dhall hiding (Text)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Network.Ethereum.Web3.Api
import           Network.Ethereum.Web3.Types
import           Numeric (readHex)
import           System.Console.CmdArgs hiding (auto)

-- TODO can I get rid of this redundant configFile param via Cmd Product Type?
data FiDCmd = Info    {config :: Text, scope :: Text}
            | Request {config :: Text, debtor :: Text, amount :: Integer}
            | Send    {config :: Text, creditor :: Text, amount :: Integer}
            deriving (Show, Data, Typeable)

--  validate these to make sure they're all valid
--  should they all be integers? why not?
--  is there aleardy an efficient uint256 type in haskell?
data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Address
                                     , debtor :: Address
                                     , amount :: Integer
                                     } deriving Show

data FiDConfig = FiDConfig { fidAddress :: Text
                           , cpAddress :: Text
                           , userAddress :: Text
                           } deriving (Show, Generic)

instance Interpret FiDConfig


main :: IO ()
main = do mode <- cmdArgs (modes [Info "" "fid", Request "" "" 0, Send "" "" 0])
          let configFilePath = config mode
          config <- input auto $ LT.fromStrict configFilePath
          runMode config mode


runMode :: FiDConfig -> FiDCmd -> IO ()
runMode config (Info _ "fid") = print =<< runWeb3 (fidLogs config)
runMode config (Info _ "user") = print =<< runWeb3 (userLogs config)
runMode _      (Info _ "all") = print =<< runWeb3 allLogs
runMode config (Send _ creditor amount) =
    print =<< runWeb3 (eth_sign (fromRight Addr.zero . Addr.fromText $ userAddress config) creditor)
runMode _ _ = putStrLn "Not yet implemented"


-- fetch all logs
-- terminal equivalent: curl -X POST --data {"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock": "0x0"}],"id":73} localhost:8545
allLogs :: Provider a => Web3 a [Change]
allLogs = eth_getLogs (Filter Nothing Nothing (Just "0x0") Nothing)


-- fetch cp logs related to FiD UCAC
-- verify that these are proper logs
fidLogs :: Provider a => FiDConfig -> Web3 a [Either String IssueCreditLog]
fidLogs config = fmap interpretUcacLog <$>
    -- TODO throw and error if `Addr.fromText` returns `Left`
    eth_getLogs (Filter (rightToMaybe . Addr.fromText $ cpAddress config)
                        Nothing
                        (Just "0x0") -- start from block 0
                        Nothing)


-- TODO throw and error if `Addr.fromText` returns `Left`
userLogs :: Provider a => FiDConfig -> Web3 a [Either String IssueCreditLog]
userLogs config = do asCreditor <- fmap interpretUcacLog <$> eth_getLogs credFilter
                     asDebtor <- fmap interpretUcacLog <$> eth_getLogs debtFilter
                     return $ asCreditor ++ asDebtor
    where filterWithTopics topics =
                       Filter (rightToMaybe . Addr.fromText $ cpAddress config)
                              (Just topics)
                              (Just "0x0")
                              Nothing
          credFilter = filterWithTopics [Nothing, Nothing, Just userAddrBytes, Nothing]
          debtFilter = filterWithTopics [Nothing, Nothing, Nothing, Just userAddrBytes]
          userAddrBytes = addressToBytes32 $ userAddress config


-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either String Address
bytes32ToAddress = Addr.fromText . T.drop 26

addressToBytes32 :: Text -> Text
addressToBytes32 = T.append "0x000000000000000000000000" . T.drop 2


hexToInteger :: Text -> Integer
hexToInteger = fst . head . readHex . dropHexPrefix . T.unpack
    where dropHexPrefix ('0' : 'x' : xs) = xs
          dropHexPrefix xs = xs


interpretUcacLog :: Change -> Either String IssueCreditLog
interpretUcacLog change = do creditorAddr <- bytes32ToAddress . (!! 2) $ changeTopics change
                             debtorAddr <- bytes32ToAddress . (!! 3) $ changeTopics change
                             pure $ IssueCreditLog (changeAddress change)
                                                   creditorAddr
                                                   debtorAddr
                                                   (hexToInteger $ changeData change)
