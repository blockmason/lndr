{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module EthInterface where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data
import           Data.Either (rights)
import           Data.Either.Combinators (fromRight, mapLeft)
import           Data.List.Safe ((!!))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import qualified Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Web3 as Web3
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
                                     , memo :: Text
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
                                   , memo :: Text
                                   , signature :: Text
                                   } deriving (Show, Generic)
$(deriveJSON defaultOptions ''CreditRecord)

data SubmissionResponse = SubmissionResponse { hash :: Text
                                     , nonce :: Integer
                                     } deriving (Show, Generic)
$(deriveJSON defaultOptions ''SubmissionResponse)

ucacId :: Text
ucacId = "0x7624778dedc75f8b322b9fa1632a610d40b85e106c7d9bf0e743a9ce291b9c6f"

ucacIdB :: BytesN 32
ucacIdB = BytesN . bytesDecode . T.take 64 . T.drop 2 $ ucacId

cpAddress :: Text
cpAddress = "0xd5ec73eac35fc9dd6c3f440bce314779fed09f60"

cpAddr :: Address
cpAddr = fromRight Addr.zero . Addr.fromText $ cpAddress


bytesDecode :: Text -> Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8


decomposeSig :: Text -> (BytesN 32, BytesN 32, BytesN 32)
decomposeSig sig = (sigR, sigS, sigV)
    where strippedSig = T.drop 2 sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = BytesN . bytesDecode . T.take 2 . T.drop 128 $ strippedSig

-- create functions to call CreditProtocol contract
[abiFrom|data/CreditProtocol.abi|]

queryNonce :: Provider a => Address -> Address -> Web3 a Integer
queryNonce = getNonce cpAddr

signCreditRecord :: CreditRecord Unsigned
                 -> ExceptT Web3Error IO (Integer, Text, CreditRecord Signed)
signCreditRecord r@(CreditRecord c d a m u) = do
            if c == d
                then throwError $ UserFail "same creditor and debtor"
                else pure ()
            ExceptT . runWeb3 $ do
                nonce <- queryNonce debtorAddr creditorAddr
                let message = T.append "0x" . T.concat $
                      stripHexPrefix <$> [ ucacId
                                         , c
                                         , d
                                         , integerToHex a
                                         , integerToHex nonce
                                         ]
                hash <- Web3.sha3 message
                sig <- Eth.sign initiatorAddr hash
                return (nonce, hash, r { signature = sig })
    where debtorAddr = textToAddress d
          creditorAddr = textToAddress c
          initiatorAddr = textToAddress u


finalizeTransaction :: Text -> Text -> CreditRecord Signed -> IO (Either Web3Error TxHash)
finalizeTransaction sig1 sig2 r@(CreditRecord c d a m _) = runWeb3 $ do
      let s1@(sig1r, sig1s, sig1v) = decomposeSig sig1
      let s2@(sig2r, sig2s, sig2v) = decomposeSig sig2
      issueCredit cpAddr (0 :: Ether) ucacIdB
                  (textToAddress c) (textToAddress d) a
                  [ sig1r, sig1s, sig1v ]
                  [ sig2r, sig2s, sig2v ]
                  (BytesN $ bytesDecode m)

-- TODO THIS CAN BE DONE IN A CLEANER WAY
-- fetch cp logs related to FiD UCAC
-- verify that these are proper logs
fidLogs :: Provider a => Web3 a [IssueCreditLog]
fidLogs = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just cpAddr)
                        Nothing
                        (Just "0x0") -- start from block 0
                        Nothing)

interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    pure $ IssueCreditLog (changeAddress change)
                          creditorAddr
                          debtorAddr
                          (hexToInteger $ changeData change)
                          "TODO fix"


-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either SomeException Address
bytes32ToAddress = mapLeft (toException . TypeError) . Addr.fromText . T.drop 26

addressToBytes32 :: Text -> Text
addressToBytes32 = T.append "0x000000000000000000000000" . T.drop 2

-- TODO keep this in either
textToAddress :: Text -> Address
textToAddress = fromRight Addr.zero . Addr.fromText

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
