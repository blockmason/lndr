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


issueCreditEvent :: Text
issueCreditEvent = "0xf3478cc0d8e00370ff63723580cb5543f72da9ca849bb45098417575c51de3cb"

-- "lndr" in ascii"
ucacId :: Text
ucacId = "0x6c6e647200000000000000000000000000000000000000000000000000000000"

ucacIdB :: BytesN 32
ucacIdB = BytesN . bytesDecode . T.take 64 . T.drop 2 $ ucacId

cpAddress :: Text
cpAddress = "0xed5ceb7730af034218d77130d96f46970f170c05"

cpAddr :: Address
cpAddr = fromRight Addr.zero . Addr.fromText $ cpAddress

bytesDecode :: Text -> Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

addrToBS = T.encodeUtf8 . Addr.toText

decomposeSig :: Text -> (BytesN 32, BytesN 32, BytesN 32)
decomposeSig sig = (sigR, sigS, sigV)
    where strippedSig = stripHexPrefix sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = BytesN . bytesDecode . alignR . T.take 2 . T.drop 128 $ strippedSig

-- create functions to call CreditProtocol contract
[abiFrom|data/CreditProtocol.abi|]


queryBalance :: Address -> IO (Either Web3Error Integer)
queryBalance = runWeb3 . fmap adjustSigned . balances cpAddr ucacIdB
    where adjustSigned x | x > div maxNeg 2 = x - maxNeg
                         | otherwise        = x
          maxNeg = 2^256


queryNonce :: Provider a => Address -> Address -> Web3 a Integer
queryNonce = getNonce cpAddr

hashCreditRecord :: forall b. Provider b => CreditRecord -> Web3 b (Integer, Text)
hashCreditRecord r@(CreditRecord creditor debtor amount _ _ _ _ _) = do
                nonce <- queryNonce creditor debtor
                let message = T.concat $
                      stripHexPrefix <$> [ ucacId
                                         , Addr.toText creditor
                                         , Addr.toText debtor
                                         , integerToHex amount
                                         , integerToHex nonce
                                         ]
                return (nonce, EU.hashText message)


finalizeTransaction :: Text -> Text -> CreditRecord -> IO (Either Web3Error TxHash)
finalizeTransaction sig1 sig2 r@(CreditRecord creditor debtor amount memo _ _ _ _) = do
      let s1@(sig1r, sig1s, sig1v) = decomposeSig sig1
          s2@(sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      runWeb3 $ issueCredit cpAddr (0 :: Ether) ucacIdB
                            creditor debtor amount
                            [ sig1r, sig1s, sig1v ]
                            [ sig2r, sig2s, sig2v ]
                            encodedMemo


lndrLogs :: Provider a => Maybe Address -> Maybe Address -> Web3 a [IssueCreditLog]
lndrLogs p1M p2M = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just cpAddr)
                        (Just [ Just issueCreditEvent, Just ucacId
                              , addressToBytes32 <$> p1M
                              , addressToBytes32 <$> p2M ])
                        (Just "0x0x46A400")
                        Nothing)


interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    pure $ IssueCreditLog (changeAddress change)
                          creditorAddr
                          debtorAddr
                          (hexToInteger . T.take 64 . stripHexPrefix $ changeData change)
                          (T.decodeUtf8 . fst . BS16.decode . T.encodeUtf8 . T.take 64 . T.drop 64 . stripHexPrefix $ changeData change)

-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either SomeException Address
bytes32ToAddress = mapLeft (toException . TypeError) . Addr.fromText . T.drop 26


addressToBytes32 :: Address -> Text
addressToBytes32 = T.append "0x" . alignR . Addr.toText


-- TODO handle the error better
textToAddress :: Text -> Address
textToAddress = fromRight (error "bad address") . Addr.fromText


hexToInteger :: Text -> Integer
hexToInteger = fst . head . readHex . T.unpack . stripHexPrefix


stripHexPrefix :: Text -> Text
stripHexPrefix x | T.isPrefixOf "0x" x = T.drop 2 x
                 | otherwise = x


integerToHex :: Integer -> Text
integerToHex x = T.append "0x" strRep
    where strRep = alignR . T.pack $ showHex x ""


align :: Text -> (Text, Text)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (T.length v `mod` 64)
        zeros = T.replicate zerosLen "0"

alignL = fst . align
alignR = snd . align
