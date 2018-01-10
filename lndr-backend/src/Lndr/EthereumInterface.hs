{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}
-- | Functions that interact with ethereum blockchain using web3.
--
-- The TemplateHaskell 'abiFrom' is used to create the 'issueCredit' function
-- at compile time. This funtion is used to submit credit records to the
-- 'CreditProtocol.sol' smart contract. NB: all functions from the smart
-- contract abi are populated, including 'getNonce', etc.

module Lndr.EthereumInterface (
      lndrLogs
    , finalizeTransaction
    , verifySettlementPayment
    , settlementDataFromCreditRecord
    ) where

import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as BS16
import           Data.Default
import           Data.Either (rights)
import           Data.List.Safe ((!!))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth as Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Prelude hiding (lookup, (!!))


-- Create functions to call CreditProtocol contract. Currently, only `issueCredit` is used.
[abiFrom|data/CreditProtocol.abi|]


finalizeTransaction :: ServerConfig -> Text -> Text -> CreditRecord
                    -> IO (Either Web3Error TxHash)
finalizeTransaction config sig1 sig2 (CreditRecord creditor debtor amount memo _ _ _ _ _ _ _) = do
      let (sig1r, sig1s, sig1v) = decomposeSig sig1
          (sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      runWeb3 $ issueCredit callVal
                            (lndrUcacAddr config)
                            creditor debtor amount
                            [ sig1r, sig1s, sig1v ]
                            [ sig2r, sig2s, sig2v ]
                            encodedMemo
    where callVal = def { callFrom = Just $ executionAddress config
                        , callTo = creditProtocolAddress config
                        , callGasPrice = Just . Quantity $ gasPrice config
                        , callValue = Just . Quantity $ 0
                        , callGas = Just . Quantity $ maxGas config
                        }


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
    ucacAddr <- bytes32ToAddress <=< (!! 1) $ changeTopics change
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    let amount = hexToInteger . takeNthByte32 0 $ changeData change
        nonce = hexToInteger . takeNthByte32 1 $ changeData change
        memo = T.decodeUtf8 . fst . BS16.decode . T.encodeUtf8 . takeNthByte32 2 $ changeData change
    pure $ IssueCreditLog ucacAddr
                          creditorAddr
                          debtorAddr
                          amount
                          nonce
                          memo


verifySettlementPayment :: Text -> Address -> Address -> Integer -> IO Bool
verifySettlementPayment txHash debtor creditor amount = do
    transactionME <- runWeb3 $ Eth.getTransactionByHash txHash
    case transactionME of
        Right (Just transaction) ->
            let fromMatch = txFrom transaction == debtor
                toMatch = txTo transaction == Just creditor
                valueMatch = hexToInteger (txValue transaction) == amount
                -- TODO bring back this value match component
            in return $ fromMatch && toMatch -- && valueMatch
        _                        -> return False


settlementDataFromCreditRecord :: CreditRecord -> IO (Maybe SettlementData)
settlementDataFromCreditRecord (CreditRecord _ _ amount _ _ _ _ _ saM scM sbnM) = case scM of
    Just currency -> do
        price <- queryEtheruemPrice
        -- assumes USD / ETH settlement for now
        let settlementAmount = floor $ fromIntegral amount * unPrice price * 10 ^ 18
        blockNumber <- currentBlockNumber
        return . Just $ SettlementData settlementAmount currency blockNumber
    Nothing -> return Nothing
