{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lndr.Util where

import           Control.Exception
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as BS16
import           Data.Either.Combinators (fromRight, mapLeft)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lndr.Types
import qualified Network.Ethereum.Util as EU
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Address as Addr
import           Numeric (readHex, showHex)

hashCreditRecord :: Address -> Nonce -> CreditRecord -> Text
hashCreditRecord ucacAddr nonce (CreditRecord creditor debtor amount _ _ _ _ _ _ _ _) =
                let message = T.concat $
                      stripHexPrefix <$> [ Addr.toText ucacAddr
                                         , Addr.toText creditor
                                         , Addr.toText debtor
                                         , integerToHex amount
                                         , integerToHex $ unNonce nonce
                                         ]
                in EU.hashText message


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


alignL :: Text -> Text
alignL = fst . align


alignR :: Text -> Text
alignR = snd . align


setUcac :: Address -> IssueCreditLog -> IssueCreditLog
setUcac lndrUcac creditlog =  creditlog { ucac = lndrUcac }

configToResponse :: ServerConfig -> ConfigResponse
configToResponse config = ConfigResponse (lndrUcacAddr config) (creditProtocolAddress config)

settlementDataFromCreditRecord :: CreditRecord -> Maybe SettlementData
settlementDataFromCreditRecord (CreditRecord _ _ _ _ _ _ _ _ saM scM sbnM) = do
    sa <- saM
    sc <- scM
    sbn <- sbnM
    return $ SettlementData sa sc sbn
