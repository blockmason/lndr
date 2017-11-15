{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lndr.EthInterface where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Concurrent.STM
import qualified Crypto.Hash as C (Digest, Keccak_256, hash)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as BS16
import           Data.Data
import           Data.Either (rights)
import           Data.Either.Combinators (fromRight, mapLeft)
import           Data.List.Safe ((!!))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable
import           GHC.Generics
import           Lndr.Types
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import qualified Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Web3 as Web3
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Numeric (readHex, showHex)
import           Prelude hiding ((!!))
import qualified STMContainers.Map as Map


freshState :: IO ServerState
freshState = ServerState <$> atomically Map.new
                         <*> atomically Map.new
                         <*> atomically Map.new

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
    where strippedSig = stripHexPrefix sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = BytesN . bytesDecode . alignR . T.take 2 . T.drop 128 $ strippedSig

-- create functions to call CreditProtocol contract
[abiFrom|data/CreditProtocol.abi|]

queryNonce :: Provider a => Address -> Address -> Web3 a Integer
queryNonce = getNonce cpAddr

hashCreditRecord :: forall a b. Provider b => CreditRecord a -> Web3 b (Integer, Text)
hashCreditRecord r@(CreditRecord c d a m u) = do
                nonce <- queryNonce debtorAddr creditorAddr
                let message = T.concat $
                      stripHexPrefix <$> [ ucacId
                                         , c
                                         , d
                                         , integerToHex a
                                         , integerToHex nonce
                                         ]
                return (nonce, EU.hashText message)
    where debtorAddr = textToAddress d
          creditorAddr = textToAddress c


signCreditRecord :: CreditRecord Unsigned
                 -> ExceptT Web3Error IO (Integer, Text, CreditRecord Signed)
signCreditRecord r@(CreditRecord c d a m u) = do
            if c == d
                then throwError $ UserFail "same creditor and debtor"
                else pure ()
            ExceptT . runWeb3 $ do
                (nonce, hash) <- hashCreditRecord r
                sig <- Eth.sign initiatorAddr hash
                return (nonce, hash, r { signature = sig })
    where debtorAddr = textToAddress d
          creditorAddr = textToAddress c
          initiatorAddr = textToAddress u


finalizeTransaction :: Text -> Text -> CreditRecord Signed -> IO (Either Web3Error TxHash)
finalizeTransaction sig1 sig2 r@(CreditRecord c d a m _) = do
      let s1@(sig1r, sig1s, sig1v) = decomposeSig sig1
          s2@(sig2r, sig2s, sig2v) = decomposeSig sig2
      runWeb3 $ issueCredit cpAddr (0 :: Ether) ucacIdB
                            (textToAddress c) (textToAddress d) a
                            [ sig1r, sig1s, sig1v ]
                            [ sig2r, sig2s, sig2v ]
                            (BytesN . bytesDecode $ alignL m)

-- TODO THIS CAN BE DONE IN A CLEANER WAY
-- fetch cp logs related to LNDR UCAC
-- verify that these are proper logs
lndrLogs :: Provider a => Web3 a [IssueCreditLog]
lndrLogs = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just cpAddr)
                        Nothing
                        (Just "0x0") -- start from block 0
                        Nothing)


lndrDebitLogs :: Provider a => Address -> Web3 a [IssueCreditLog]
lndrDebitLogs addr = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just cpAddr)
                        (Just [Nothing, Nothing, Just (addressToBytes32 addr)])
                        (Just "0x0") -- start from block 0
                        Nothing)


lndrCreditLogs :: Provider a => Address -> Web3 a [IssueCreditLog]
lndrCreditLogs addr = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just cpAddr)
                        (Just [Nothing, Just (addressToBytes32 addr)])
                        (Just "0x0") -- start from block 0
                        Nothing)


interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    pure $ IssueCreditLog (changeAddress change)
                          creditorAddr
                          debtorAddr
                          (hexToInteger . T.take 64 . stripHexPrefix $ changeData change)
                          (T.take 64 . T.drop 64 . stripHexPrefix $ changeData change)

-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either SomeException Address
bytes32ToAddress = mapLeft (toException . TypeError) . Addr.fromText . T.drop 26

addressToBytes32 :: Address -> Text
addressToBytes32 = alignR . Addr.toText

-- TODO keep this in either
textToAddress :: Text -> Address
textToAddress = fromRight Addr.zero . Addr.fromText


hexToInteger :: Text -> Integer
hexToInteger = fst . head . readHex . T.unpack . stripHexPrefix


stripHexPrefix :: Text -> Text
stripHexPrefix x | T.isPrefixOf "0x" x = T.drop 2 x
                 | otherwise = x


integerToHex :: Integer -> Text
integerToHex x = T.append "0x" strRep
    where strRep = alignR . T.pack $ showHex x ""


hashPrefixedMessage :: String -> Text -> Text
hashPrefixedMessage pre message = T.pack . show $ keccakDigest
    where messageBytes = fst . BS16.decode . T.encodeUtf8 $ message
          prefix = T.encodeUtf8 . T.pack $
            pre ++ show (B.length messageBytes)
          keccakDigest :: C.Digest C.Keccak_256
          keccakDigest = C.hash (prefix `B.append` messageBytes)


align :: Text -> (Text, Text)
align v = (v <> zeros, zeros <> v)
  where zerosLen | T.length v `mod` 64 == 0 = 0
                 | otherwise                = 64 - (T.length v `mod` 64)
        zeros = T.replicate zerosLen "0"

alignL = fst . align
alignR = snd . align
