{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Lndr.Util where

import           Control.Exception
import           Control.Lens
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base16        as BS16
import           Data.Either.Combinators       (fromRight, mapLeft)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Lndr.Types
import qualified Network.Ethereum.Util         as EU
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Numeric                       (readHex, showHex)


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


decomposeSig :: Text -> (BytesN 32, BytesN 32, BytesN 32)
decomposeSig sig = (sigR, sigS, sigV)
    where strippedSig = stripHexPrefix sig
          sigR = BytesN . bytesDecode $ T.take 64 strippedSig
          sigS = BytesN . bytesDecode . T.take 64 . T.drop 64 $ strippedSig
          sigV = BytesN . bytesDecode . alignR . T.take 2 . T.drop 128 $ strippedSig


bytesDecode :: Text -> BA.Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8


bytesEncode :: Text -> Text
bytesEncode = T.decodeUtf8 . BS16.encode . T.encodeUtf8


textToBytesN32 :: Text -> BytesN 32
textToBytesN32 = BytesN . bytesDecode . T.take 64 . T.drop 2


addrToBS :: Address -> B.ByteString
addrToBS = T.encodeUtf8 . Addr.toText


takeNthByte32 :: Int -> Text -> Text
takeNthByte32 n = T.take byte32CharLength . T.drop (n * byte32CharLength) . stripHexPrefix
    where byte32CharLength = 64


-- transforms the standard ('0x' + 64-char) bytes32 rendering of a log field into the
-- 40-char hex representation of an address
bytes32ToAddress :: Text -> Either SomeException Address
bytes32ToAddress = mapLeft (toException . TypeError) . Addr.fromText . T.drop 26


addressToBytes32 :: Address -> Text
addressToBytes32 = T.append "0x" . alignR . Addr.toText


textToAddress :: Text -> Address
textToAddress = fromRight (error "bad address") . Addr.fromText


hexToInteger :: Text -> Integer
hexToInteger = fst . head . readHex . T.unpack . stripHexPrefix


stripHexPrefix :: Text -> Text
stripHexPrefix x | T.isPrefixOf "0x" x = T.drop 2 x
                 | otherwise           = x


addHexPrefix :: Text -> Text
addHexPrefix x | T.isPrefixOf "0x" x = x
               | otherwise           = T.append "0x" x


integerToHex :: Integer -> Text
integerToHex x = T.append "0x" strRep
    where strRep = alignR . T.pack $ showHex x ""


integerToHex' :: Integer -> Text
integerToHex' x = T.append "0x" . T.pack $ showHex x ""


align :: Text -> (Text, Text)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (T.length v `mod` 64)
        zeros = T.replicate zerosLen "0"


alignL :: Text -> Text
alignL = fst . align


alignR :: Text -> Text
alignR = snd . align


getUcac :: M.Map Text Address -> Maybe Text -> Address
getUcac ucacAddresses currency =
    let defaultUcac = fromMaybe (error "no USD ucac registered") $ M.lookup "USD" ucacAddresses
    in fromMaybe defaultUcac $ (`M.lookup` ucacAddresses) =<< currency


configToResponse :: ServerConfig -> ConfigResponse
configToResponse config = ConfigResponse (lndrUcacAddrs config) (creditProtocolAddress config)
                                         (gasPrice config) (ethereumPrices config)
                                         (latestBlockNumber config - 40600)


-- example input data: 0x0a5b410e000000000000000000000000869a8f2c3d22be392618ed06c8f548d1d5b5aed600000000000000000000000070d71994d0414c19c1f09f1f2946544e8d97c4290000000000000000000000001b5fec5060e51886184d30b3d211d50836087b83000000000000000000000000000000000000000000000000000000000000006481e2e0f119561515281f1c76d64431760a8612305931f5378f3004da3aa6209927b3f24c58888a06da343aa4bf1f3520122e703e6ccf146ee9ac60e2123b10d8000000000000000000000000000000000000000000000000000000000000001bb6ff2d00768200c769018c277d53b2e2a3a3d5f4d740a2acc3e35cb4421dd38a45a02960d1e92cf061a357a819072658a482c02c4a4ee06e01cb58cf66bc3a8f000000000000000000000000000000000000000000000000000000000000001c736f6d657468696e672020202020202020202020202020202020202020202020
parseIssueCreditInput :: Nonce -> Text -> (IssueCreditLog, Text, Bool, Text, Bool)
parseIssueCreditInput (Nonce nonce) inputData = ( creditLog
                                                , creditorSig
                                                , creditorSigValid
                                                , debtorSig
                                                , debtorSigValid
                                                )
    where (funcIdText, rest) = T.splitAt 8 $ stripHexPrefix inputData
          (ucacIdText, rest1) = T.splitAt 64 rest
          (creditorText', rest2) = T.splitAt 64 rest1
          creditorText = T.drop 24 creditorText'
          (debtorText', rest3) = T.splitAt 64 rest2
          debtorText = T.drop 24 debtorText'
          (amountText, rest4) = T.splitAt 64 rest3
          (creditorSig', rest5) = T.splitAt 192 rest4
          parseSig (x, y) = T.append x $ T.drop 62 y
          creditorSig = parseSig $ T.splitAt 128 creditorSig'
          (debtorSig', rest6) = T.splitAt 192 rest5
          debtorSig = parseSig $ T.splitAt 128 debtorSig'
          memo = T.decodeUtf8 . fst . BS16.decode $ T.encodeUtf8 $ T.take 64 rest6
          creditLog = IssueCreditLog (textToAddress $ T.drop 24 ucacIdText)
                                     (textToAddress creditorText)
                                     (textToAddress debtorText)
                                     (hexToInteger amountText)
                                     nonce
                                     memo
          messageHash = EU.hashPersonalMessage $ hashCreditLog creditLog
          creditorSigValid = Right creditorText == EU.ecrecover creditorSig messageHash
          debtorSigValid = Right debtorText == EU.ecrecover debtorSig messageHash
