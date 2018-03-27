{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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
      lndrWeb3
    , lndrLogs
    , finalizeTransaction
    , verifySettlementPayment

    -- * functions defined via TH rendering of solidity ABI
    , getNonce
    , balances
    , createAndStakeUcac
    , currentBlockNumber
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

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.Bimap                  as B
import qualified Data.ByteArray              as BA
import qualified Data.ByteString.Base16      as BS16
import           Data.Default
import           Data.Either                 (rights)
import           Data.List.Safe              ((!!))
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Sized                  hiding (fmap, (!!), (++))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Tuple
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Handler.Types
import           Lndr.Util
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth   as Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Prelude                     hiding (lookup, (!!))


-- Create functions to call CreditProtocol contract.
[abiFrom|data/CreditProtocol.abi|]


lndrWeb3 :: Web3 b -> LndrHandler b
lndrWeb3 web3Action = do
    configTVar <- asks serverConfig
    config <- liftIO . atomically $ readTVar configTVar
    let provider = HttpProvider (web3Url config)
    ioEitherToLndr $ runWeb3' provider web3Action


-- | Submit a bilateral credit record to the Credit Protocol smart contract.
finalizeTransaction :: ServerConfig -> BilateralCreditRecord
                    -> LndrHandler TxHash
finalizeTransaction config (BilateralCreditRecord (CreditRecord creditor debtor amount memo _ _ _ _ ucac _ _ _) sig1 sig2 _) = do
      let (sig1r, sig1s, sig1v) = decomposeSig sig1
          (sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      lndrWeb3 $ issueCredit callVal
                             ucac
                             creditor debtor (UIntN amount)
                             (sig1r :< sig1s :< sig1v :< NilL)
                             (sig2r :< sig2s :< sig2v :< NilL)
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
lndrLogs :: ServerConfig -> Text -> Maybe Address -> Maybe Address
         -> Web3 [IssueCreditLog]
lndrLogs config currencyKey creditorM debtorM = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just $ creditProtocolAddress config)
                        (Just [ Just (issueCreditEvent config)
                              -- TODO this will have to change once we deploy
                              -- multiple lndr ucacs
                              , addressToBytes32 <$> B.lookup currencyKey (lndrUcacAddrs config)
                              , addressToBytes32 <$> creditorM
                              , addressToBytes32 <$> debtorM ])
                        (BlockWithNumber . BlockNumber $ scanStartBlock config)
                        Latest)


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
verifySettlementPayment :: BilateralCreditRecord -> LndrHandler ()
verifySettlementPayment (BilateralCreditRecord creditRecord _ _ (Just txHash)) = do
    transactionM <- lndrWeb3 . Eth.getTransactionByHash $ addHexPrefix txHash
    case transactionM of
        (Just transaction) ->
            let fromMatch = txFrom transaction == creditor creditRecord
                toMatch = txTo transaction == Just (debtor creditRecord)
                transactionValue = hexToInteger $ txValue transaction
                settlementValue = fromMaybe 0 $ settlementAmount creditRecord
                valueMatch = transactionValue == settlementValue
                creditHash = T.unpack $ hash creditRecord
            in case (fromMatch, toMatch, valueMatch) of
                (False, _, _)      -> lndrError $ "Bad from match, hash: " ++ creditHash
                (_, False, _)      -> lndrError $ "Bad to match, hash: " ++ creditHash
                (_, _, False)      -> lndrError $ "Bad value match, hash: " ++ creditHash
                                                 ++ "tx value: " ++ show transactionValue
                                                 ++ ", settlementValue: " ++ show settlementValue
                (True, True, True) -> pure ()
        Nothing -> lndrError $ "transaction not found, tx_hash: " ++ T.unpack txHash
verifySettlementPayment _ = lndrError "Incompelete settlement record"


-- | Queries the blockchain for current blocknumber.
currentBlockNumber :: ServerConfig -> MaybeT IO Integer
currentBlockNumber config = do
    let provider = HttpProvider (web3Url config)
    blockNumberTextE <- runWeb3' provider Eth.blockNumber
    return $ case blockNumberTextE of
        Right (BlockNumber number) -> number
        Left _        -> 0
