{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Either (rights)
import           Data.Either.Combinators (rightToMaybe, fromRight, mapLeft)
import           Data.List.Safe ((!!))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Dhall hiding (Text)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Network.Ethereum.Web3.Api
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Numeric (readHex, showHex)
import           Prelude hiding ((!!))
import           System.Console.CmdArgs hiding (auto)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as BS16

import Debug.Trace

bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

-- Contract:
--         Constructor (address,uint256,uint256)
--         Events:
--                 IssueCredit(bytes32,address,address,uint256)
--                 UcacCreation(bytes32,address,bytes32)
--                 OwnershipTransferred(address,address)
--         Methods:
--                 0x110d9221 ucacs(bytes32)
--                 0x13c506dc stakeTokens(bytes32,address,uint256)
--                 0x1752bb6c txPerGigaTokenPerHour()
--                 0x20c4410e setTokensToOwnUcac(uint256)
--                 0x2d473b26 issueCredit(bytes32,address,address,uint256,bytes32,bytes32,uint8,bytes32,bytes32,uint8)
--                 0x444a49d3 createAndStakeUcac(address,bytes32,bytes32,uint256)
--                 0x5af41713 Stake(address,uint256,uint256)
--                 0x5f7772c9 getUcacAddr(bytes32)
--                 0x8da5cb5b owner()
--                 0x9333fbda nonces(address,address)
--                 0x97a2f01b unstakeTokens(bytes32,uint256)
--                 0x9eac6024 stakedTokensMap(bytes32,address)
--                 0xb4c89ec0 executeUcacTx(bytes32)
--                 0xbb2d3208 setTxPerTokenPerHour(uint256)
--                 0xd828435d getNonce(address,address)
--                 0xd93d7361 balances(bytes32,address)
--                 0xe4b08555 currentTxLevel(bytes32)
--                 0xea5a2cc2 tokensToOwnUcac()
--                 0xf2fde38b transferOwnership(address)
--                 0xfc0c546a token()

[abiFrom|data/CreditProtocol.abi|]
-- [abiFrom|data/ERC20.json|]

-- TODO can I get rid of this redundant configFile param via Cmd Product Type?
data FiDCmd = Info    {config :: Text, scope :: Text}
            | Request {config :: Text, debtor :: Text, amount :: Integer}
            | Send    {config :: Text, creditor :: Text, amount :: Integer}
            | Nonce   {config :: Text, counterparty :: Text}
            | Test    {config :: Text}
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
                           , fidUcacId :: Text
                           , cpAddress :: Text
                           , userAddress :: Text
                           } deriving (Show, Generic)

instance Interpret FiDConfig

-- getNonce

--     // id -> id -> # of transactions in all UCACs
--     // lesser id is must always be the first argument
--     mapping(address => mapping(address => uint256)) public nonces;
--     // the standard prefix appended to 32-byte-long messages when signed by an
--     // Ethereum client
--     bytes prefix = "\x19Ethereum Signed Message:\n32";
--
--     event IssueCredit(bytes32 indexed ucac, address indexed creditor, address indexed debtor, uint256 amount);
--     function getNonce(address p1, address p2) public constant returns (uint256) {
--         return p1 < p2 ? nonces[p1][p2] : nonces[p2][p1];
--     }

main :: IO ()
main = do mode <- cmdArgs (modes [ Info "" "fid"
                                 , Request "" "" 0
                                 , Send "" "" 0
                                 , Nonce "" ""
                                 , Test ""
                                 ])
          -- print =<< runWeb3 eth_protocolVersion
          let configFilePath = config mode
          config <- input auto $ LT.fromStrict configFilePath
          runMode config mode


runMode :: FiDConfig -> FiDCmd -> IO ()
runMode config (Info _ "fid") = print =<< runWeb3 (fidLogs config)
runMode config (Info _ "user") = print =<< runWeb3 (userLogs config)
runMode _      (Info _ "all") = print =<< runWeb3 allLogs
-- stack exec -- fiddy send --amount 10 --config "../test/config1" --creditor=0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf
runMode config (Send _ creditorAddress sendAmount) = do
    message <- runWeb3 $ do nonce <- getNonce cpAddr senderAddr creditorAddr
                            let message = T.append "0x" . T.concat $
                                  stripHexPrefix <$> [ fidAddress config
                                                     , creditorAddress
                                                     , userAddress config
                                                     , integerToHex sendAmount
                                                     , integerToHex nonce
                                                     ]
                            hash <- web3_sha3 message
                            sig <- eth_sign senderAddr hash
                            txReceipt <- issueCredit cpAddr
                                                     (0 :: Ether)
                                                     ucacId
                                                     creditorAddr
                                                     senderAddr
                                                     sendAmount
                                                     caBS
                                                     caBS
                                                     0
                                                     caBS
                                                     caBS
                                                     0
-- issueCredit
--   :: (Unit t0, Provider p) =>
--      Address
--      -> t0
--      -> Bytes32
--      -> Address
--      -> Address
--      -> Integer
--      -> Bytes32
--      -> Bytes32
--      -> Integer
--      -> Bytes32
--      -> Bytes32
--      -> Integer
--      -> Web3 p TxHash
-- function issueCredit( bytes32 ucac, address creditor, address debtor, uint256 amount
--                     , bytes32 sig1r, bytes32 sig1s, uint8 sig1v
--                     , bytes32 sig2r, bytes32 sig2s, uint8 sig2v
--                     ) public {
                            return (message, hash, sig, txReceipt)
    print message
    where senderAddr = fromRight Addr.zero . Addr.fromText $ userAddress config
          creditorAddr = fromRight Addr.zero . Addr.fromText $ creditorAddress
          cpAddr = fromRight Addr.zero . Addr.fromText $ cpAddress config
          caBS = (Bytes32 . bytesDecode $ creditorAddress)
          ucacId = (Bytes32 . bytesDecode $ fidUcacId config)
runMode config (Nonce _ _) = print =<< runWeb3 (getNonce fidAddr senderAddr senderAddr)
    where call = Call Nothing
                      (fromRight Addr.zero . Addr.fromText $ cpAddress config)
                      Nothing
                      Nothing
                      Nothing
                      Nothing -- Tuple of the creditor and debtor ordered appropriately
          senderAddr = fromRight Addr.zero . Addr.fromText $ userAddress config
          fidAddr = fromRight Addr.zero . Addr.fromText $ cpAddress config
runMode config (Test _) = putStrLn "Nothing to see"
runMode _ _ = putStrLn "Not yet implemented"


-- fetch all logs
-- terminal equivalent: curl -X POST --data {"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock": "0x0"}],"id":73} localhost:8545
allLogs :: Provider a => Web3 a [Change]
allLogs = eth_getLogs (Filter Nothing Nothing (Just "0x0") Nothing)


-- fetch cp logs related to FiD UCAC
-- verify that these are proper logs
fidLogs :: Provider a => FiDConfig -> Web3 a [IssueCreditLog]
fidLogs config = rights . fmap interpretUcacLog <$>
    -- TODO throw and error if `Addr.fromText` returns `Left`
    eth_getLogs (Filter (rightToMaybe . Addr.fromText $ cpAddress config)
                        Nothing
                        (Just "0x0") -- start from block 0
                        Nothing)


-- TODO throw and error if `Addr.fromText` returns `Left`
userLogs :: Provider a => FiDConfig -> Web3 a [IssueCreditLog]
userLogs config = do asCreditor <- rights . fmap interpretUcacLog <$> eth_getLogs credFilter
                     asDebtor <- rights . fmap interpretUcacLog <$> eth_getLogs debtFilter
                     return $ asCreditor ++ asDebtor
    where filterWithTopics topics =
                       Filter (rightToMaybe . Addr.fromText $ cpAddress config)
                              (Just topics)
                              (Just "0x0")
                              Nothing
          credFilter = filterWithTopics [Nothing, Nothing, Just userAddrBytes, Nothing]
          debtFilter = filterWithTopics [Nothing, Nothing, Nothing, Just userAddrBytes]
          userAddrBytes = addressToBytes32 $ userAddress config


interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
                             debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
                             pure $ IssueCreditLog (changeAddress change)
                                                   creditorAddr
                                                   debtorAddr
                                                   (hexToInteger $ changeData change)


-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either SomeException Address
bytes32ToAddress = mapLeft (toException . TypeError) . Addr.fromText . T.drop 26

addressToBytes32 :: Text -> Text
addressToBytes32 = T.append "0x000000000000000000000000" . T.drop 2


hexToInteger :: Text -> Integer
hexToInteger = fst . head . readHex . dropHexPrefix . T.unpack
    where dropHexPrefix ('0' : 'x' : xs) = xs
          dropHexPrefix xs = xs

stripHexPrefix :: Text -> Text
stripHexPrefix x | T.isPrefixOf "0x" x = T.drop 2 x
                 | otherwise = x

integerToHex :: Integer -> Text
integerToHex x = T.pack strRep'
    where strRep = showHex x ""
          strRep' = "0x" ++ replicate (64 - length strRep) '0' ++ strRep
