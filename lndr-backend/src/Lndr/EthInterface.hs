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
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Data
import           Data.Either (rights)
import           Data.Either.Combinators (fromRight, mapLeft)
import           Data.List.Safe ((!!))
import           Data.Maybe (fromJust)
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
import           Prelude hiding (lookup, (!!))
import           System.FilePath


-- TODO do this with error handling & check if configurator supports loading
-- datatypes
loadConfig :: IO ServerConfig
loadConfig = do config <- load [Required $ "data" </> "lndr-server.config"]
                lndrUcacAddr <- fromJust <$> lookup config "lndrUcac"
                cpAddr <- fromJust <$> lookup config "creditProtocolAddress"
                issueCreditEvent <- fromJust <$> lookup config "issueCreditEvent"
                scanStartBlock <- fromJust <$> lookup config "scanStartBlock"
                return $ ServerConfig (textToAddress lndrUcacAddr)
                                      (textToAddress cpAddr)
                                      issueCreditEvent
                                      scanStartBlock


bytesDecode :: Text -> Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8


textToBytesN32 :: Text -> BytesN 32
textToBytesN32 = BytesN . bytesDecode . T.take 64 . T.drop 2


addrToBS :: Address -> B.ByteString
addrToBS = T.encodeUtf8 . Addr.toText


decomposeSig :: Text -> (BytesN 32, BytesN 32, BytesN 32)
decomposeSig sig = (sigR, sigS, sigV)
    where strippedSig = stripHexPrefix sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = BytesN . bytesDecode . alignR . T.take 2 . T.drop 128 $ strippedSig

-- create functions to call CreditProtocol contract
[abiFrom|data/CreditProtocol.abi|]


queryBalance :: ServerConfig -> Address -> IO (Either Web3Error Integer)
queryBalance config userAddress = runWeb3 . fmap adjustSigned $ balances (creditProtocolAddress config) (lndrUcacAddr config) userAddress
    -- TODO fix this issue in `hs-web3`
    where adjustSigned x | x > div maxNeg 2 = x - maxNeg
                         | otherwise        = x
          maxNeg = 2^256


queryNonce :: Provider a => ServerConfig -> Address -> Address -> Web3 a Integer
queryNonce config = getNonce (creditProtocolAddress config)


hashCreditRecord :: forall b. Provider b => ServerConfig -> CreditRecord -> Web3 b (Integer, Text)
hashCreditRecord config r@(CreditRecord creditor debtor amount _ _ _ _ _) = do
                nonce <- queryNonce config creditor debtor
                let message = T.concat $
                      stripHexPrefix <$> [ Addr.toText (lndrUcacAddr config)
                                         , Addr.toText creditor
                                         , Addr.toText debtor
                                         , integerToHex amount
                                         , integerToHex nonce
                                         ]
                return (nonce, EU.hashText message)


hashCreditLog :: IssueCreditLog -> Text
hashCreditLog (IssueCreditLog ucac creditor debtor amount nonce _) =
                let message = T.concat $
                      stripHexPrefix <$> [ Addr.toText ucac
                                         , Addr.toText creditor
                                         , Addr.toText debtor
                                         , integerToHex amount
                                         , integerToHex nonce
                                         ]
                in EU.hashText message


finalizeTransaction :: ServerConfig -> Text -> Text -> CreditRecord
                    -> IO (Either Web3Error TxHash)
finalizeTransaction config sig1 sig2 r@(CreditRecord creditor debtor amount memo _ _ _ _) = do
      let s1@(sig1r, sig1s, sig1v) = decomposeSig sig1
          s2@(sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      runWeb3 $ issueCredit (creditProtocolAddress config) (0 :: Ether)
                            (lndrUcacAddr config)
                            creditor debtor amount
                            [ sig1r, sig1s, sig1v ]
                            [ sig2r, sig2s, sig2v ]
                            encodedMemo


lndrLogs :: Provider a => ServerConfig -> Maybe Address -> Maybe Address
         -> Web3 a [IssueCreditLog]
lndrLogs config p1M p2M = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just $ creditProtocolAddress config)
                        (Just [ Just (issueCreditEvent config)
                              , Just (addressToBytes32 $ lndrUcacAddr config)
                              , addressToBytes32 <$> p1M
                              , addressToBytes32 <$> p2M ])
                        (Just . integerToHex' $ scanStartBlock config)
                        Nothing)


interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    pure $ IssueCreditLog (changeAddress change)
                          creditorAddr
                          debtorAddr
                          -- TODO clean this up
                          (hexToInteger . T.take 64 . stripHexPrefix $ changeData change)
                          (hexToInteger . T.take 64 . T.drop 64 . stripHexPrefix $ changeData change)
                          (T.decodeUtf8 . fst . BS16.decode . T.encodeUtf8 . T.take 64 . T.drop 128 . stripHexPrefix $ changeData change)

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


integerToHex' :: Integer -> Text
integerToHex' x = T.append "0x" . T.pack $ showHex x ""


align :: Text -> (Text, Text)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (T.length v `mod` 64)
        zeros = T.replicate zerosLen "0"

alignL = fst . align
alignR = snd . align
