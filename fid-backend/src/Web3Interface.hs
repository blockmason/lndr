{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module Web3Interface where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data
import           Data.Either (rights)
import           Data.Either.Combinators (rightToMaybe, fromRight, mapLeft)
import           Data.List.Safe ((!!))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Network.Ethereum.Web3.Api
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Numeric (readHex, showHex)
import           Prelude hiding ((!!))
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as BS16

data IssueCreditLog = IssueCreditLog { ucac :: Address
                                     , creditor :: Address
                                     , debtor :: Address
                                     , amount :: Integer
                                     } deriving Show
$(deriveJSON defaultOptions ''IssueCreditLog)

-- Types that populate `CreditRecord`'s phantom type field
data Signed = Signed
$(deriveJSON defaultOptions ''Signed)
data Unsigned = Unsigned
$(deriveJSON defaultOptions ''Unsigned)

-- `a` is a phantom type that indicates whether a record has been signed or not
data CreditRecord a = CreditRecord { creditor :: Text
                                   , debtor :: Text
                                   , amount :: Integer
                                   , signature :: Text
                                   } deriving (Show, Generic)
$(deriveJSON defaultOptions ''CreditRecord)

data ServerResponse = ServerResponse { code :: Int } deriving (Show, Generic)
$(deriveJSON defaultOptions ''ServerResponse)

bytesDecode :: Text -> Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

decomposeSig :: Text -> (BytesN 32, BytesN 32, Integer)
decomposeSig sig = (sigR, sigS, sigV)
    where strippedSig = T.drop 2 sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = hexToInteger . T.take 2 . T.drop 128 $ strippedSig

[abiFrom|data/CreditProtocol.abi|]

-- runMode :: FiDConfig -> FiDCmd -> IO ()
-- runMode config (Info _ "fid") = print =<< runWeb3 (fidLogs config)
-- runMode config (Info _ "user") = print =<< runWeb3 (userLogs config)
-- runMode _      (Info _ "all") = print =<< runWeb3 allLogs
-- runMode config (Send _ creditorAddress sendAmount) = do
--     message <- runWeb3 $ do nonce <- getNonce cpAddr senderAddr creditorAddr
--                             let message = T.append "0x" . T.concat $
--                                   stripHexPrefix <$> [ fidUcacId config
--                                                      , creditorAddress
--                                                      , userAddress config
--                                                      , integerToHex sendAmount
--                                                      , integerToHex nonce
--                                                      ]
--                             hash <- web3_sha3 message
--                             sig1 <- eth_sign creditorAddr hash
--                             sig2 <- eth_sign senderAddr hash
--                             let s1@(sig1r, sig1s, sig1v) = decomposeSig sig1
--                             let s2@(sig2r, sig2s, sig2v) = decomposeSig sig2
--                             txReceipt <- issueCredit cpAddr
--                                                      (0 :: Ether)
--                                                      ucacId
--                                                      creditorAddr
--                                                      senderAddr
--                                                      sendAmount
--                                                      sig1r
--                                                      sig1s
--                                                      sig1v
--                                                      sig2r
--                                                      sig2s
--                                                      sig2v
--                             return (sig1, sig2, txReceipt)
--     print message
--     where senderAddr = fromRight Addr.zero . Addr.fromText $ userAddress config
--           creditorAddr = fromRight Addr.zero . Addr.fromText $ creditorAddress
--           cpAddr = fromRight Addr.zero . Addr.fromText $ cpAddress config
--           ucacId :: BytesN 32
--           ucacId = BytesN . bytesDecode . T.take 64 . T.drop 2 $ fidUcacId config

-- fetch all logs
-- terminal equivalent: curl -X POST --data {"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock": "0x0"}],"id":73} localhost:8545
allLogs :: Provider a => Web3 a [Change]
allLogs = eth_getLogs (Filter Nothing Nothing (Just "0x0") Nothing)


-- TODO THIS CAN BE DONE IN A CLEANER WAY
-- fetch cp logs related to FiD UCAC
-- verify that these are proper logs
fidLogs :: Provider a => Web3 a [IssueCreditLog]
fidLogs = rights . fmap interpretUcacLog <$>
    eth_getLogs (Filter (rightToMaybe . Addr.fromText $ cpAddr)
                        Nothing
                        (Just "0x0") -- start from block 0
                        Nothing)
    where cpAddr = "0xd5ec73eac35fc9dd6c3f440bce314779fed09f60"

-- TODO throw and error if `Addr.fromText` returns `Left`
-- userLogs :: Provider a => FiDConfig -> Web3 a [IssueCreditLog]
-- userLogs config = do asCreditor <- rights . fmap interpretUcacLog <$> eth_getLogs credFilter
--                      asDebtor <- rights . fmap interpretUcacLog <$> eth_getLogs debtFilter
--                      return $ asCreditor ++ asDebtor
--     where filterWithTopics topics =
--                        Filter (rightToMaybe . Addr.fromText $ cpAddress config)
--                               (Just topics)
--                               (Just "0x0")
--                               Nothing
--           credFilter = filterWithTopics [Nothing, Nothing, Just userAddrBytes, Nothing]
--           debtFilter = filterWithTopics [Nothing, Nothing, Nothing, Just userAddrBytes]
--           userAddrBytes = addressToBytes32 $ userAddress config


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
