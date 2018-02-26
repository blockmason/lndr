{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
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
    , calculateSettlementCreditRecord

    -- * functions defined via TH rendering of solidity ABI
    , getNonce
    , balances
    , createAndStakeUcac
    , currentTxLevel
    , executeUcacTx
    , nonces
    , owner
    , setTokensToOwnUcac
    , setTxPerGigaTokenPerHour
    , stakeTokens
    , stakedTokensMap
    , token
    , tokensToOwnUcac
    , transferOwnership
    , txPerGigaTokenPerHour
    , ucacs
    , unstakeTokens
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteArray              as BA
import qualified Data.ByteString.Base16      as BS16
import           Data.Default
import           Data.Either                 (rights)
import           Data.List.Safe              ((!!))
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Util
import           Lndr.Web3
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth   as Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Prelude                     hiding (lookup, (!!))


-- Create functions to call CreditProtocol contract.
[abiFrom|data/CreditProtocol.abi|]

-- | Submit a bilateral credit record to the Credit Protocol smart contract.
finalizeTransaction :: ServerConfig -> BilateralCreditRecord
                    -> IO (Either Web3Error TxHash)
finalizeTransaction config (BilateralCreditRecord (CreditRecord creditor debtor amount memo _ _ _ _ ucac _ _ _) sig1 sig2 _) = do
      let (sig1r, sig1s, sig1v) = decomposeSig sig1
          (sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      runLndrWeb3 $ issueCredit callVal
                            ucac
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


-- | Scan blockchain for 'IssueCredit' events emitted by the Credit Protocol
-- smart contract. If 'Just addr' values are passed in for either 'creditorM'
-- or 'debtorM', or both, logs are filtered to show matching results.
lndrLogs :: Provider a => ServerConfig -> Maybe Address -> Maybe Address
         -> Web3 a [IssueCreditLog]
lndrLogs config creditorM debtorM = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just $ creditProtocolAddress config)
                        (Just [ Just (issueCreditEvent config)
                              -- TODO this will have to change once we deploy
                              -- multiple lndr ucacs
                              , addressToBytes32 <$> M.lookup "USD" (lndrUcacAddrs config)
                              , addressToBytes32 <$> creditorM
                              , addressToBytes32 <$> debtorM ])
                        (Just . integerToHex' $ scanStartBlock config)
                        Nothing)


-- | Parse a log 'Change' into an 'IssueCreditLog' if possible.
interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    ucacAddr <- bytes32ToAddress <=< (!! 1) $ changeTopics change
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    let amount = hexToInteger . takeNthByte32 0 $ changeData change
        nonce = hexToInteger . takeNthByte32 1 $ changeData change
        memo = T.decodeUtf8 . fst . BS16.decode
                            . T.encodeUtf8 . takeNthByte32 2 $ changeData change
    pure $ IssueCreditLog ucacAddr
                          creditorAddr
                          debtorAddr
                          amount
                          nonce
                          memo


-- | Verify that a settlement payment was made using a 'txHash' corresponding to
-- an Ethereum transaction on the blockchain and the associated addresses and
-- eth settlment amount.
verifySettlementPayment :: BilateralCreditRecord -> IO Bool
verifySettlementPayment (BilateralCreditRecord creditRecord _ _ (Just txHash)) = do
    transactionME <- runLndrWeb3 . Eth.getTransactionByHash $ addHexPrefix txHash
    case transactionME of
        Right (Just transaction) ->
            let fromMatch = txFrom transaction == creditor creditRecord
                toMatch = txTo transaction == Just (debtor creditRecord)
                valueMatch = Just (hexToInteger $ txValue transaction) == settlementAmount creditRecord
            in return $ fromMatch && toMatch && valueMatch
        _                        -> pure False
verifySettlementPayment _ = pure False


calculateSettlementCreditRecord :: ServerConfig -> CreditRecord -> IO CreditRecord
calculateSettlementCreditRecord config cr@(CreditRecord _ _ amount _ _ _ _ _ _ _ (Just currency) _) = do
    Just blockNumber <- runMaybeT currentBlockNumber
    let prices = ethereumPrices config
    -- assumes USD / ETH settlement for now
    -- 10 ^ 16 instead of 10 ^ 18 because our amounts are stored in cents, not
    -- dollars, so we have to divide by 100
        settlementAmount = floor $ fromIntegral amount / usd prices * 10 ^ 16

    pure (cr { settlementAmount = Just settlementAmount
             , settlementBlocknumber = Just blockNumber
             })
calculateSettlementCreditRecord _ cr@(CreditRecord _ _ _ _ _ _ _ _ _ _ Nothing _) = return cr
