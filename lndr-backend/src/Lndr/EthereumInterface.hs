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
    , currentExecutionNonce
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
import qualified Data.Bimap                   as B
import qualified Data.ByteArray               as BA
import qualified Data.ByteString.Base16       as BS16
import           Data.Default
import           Data.Either                  (rights)
import           Data.List.Safe               ((!!))
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe, maybe)
import           Data.Sized                   hiding (fmap, (!!), (++))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Tuple
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Lndr.Handler.Types
import           Lndr.Util
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth    as Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import           Network.Ethereum.Transaction (createRawTransaction)
import           Prelude                      hiding (lookup, (!!))
import           Servant


-- Create functions to call CreditProtocol contract.
[abiFrom|data/CreditProtocol.abi|]


lndrWeb3 :: Web3 b -> LndrHandler b
lndrWeb3 web3Action = do
    configTVar <- asks serverConfig
    config <- liftIO . atomically $ readTVar configTVar
    let provider = HttpProvider (web3Url config)
    ioEitherToLndr $ runWeb3' provider web3Action


-- | Submit a bilateral credit record to the Credit Protocol smart contract.
finalizeTransaction :: TVar ServerConfig -> BilateralCreditRecord
                    -> LndrHandler TxHash
finalizeTransaction configTVar (BilateralCreditRecord (CreditRecord creditor debtor amount memo _ _ _ _ ucac _ _ _) sig1 sig2 _) = do

      config <- liftIO . atomically $ do
        config <- readTVar configTVar
        -- increment the execution account's nonce
        modifyTVar' configTVar (\x -> x { executionNonce = succ (executionNonce config)})
        pure config

      let execNonce = executionNonce config
          callVal = def { callFrom = Just $ executionAddress config
                        , callTo = creditProtocolAddress config
                        , callGasPrice = Just . Quantity $ gasPrice config
                        , callValue = Just . Quantity $ 0
                        , callGas = Just . Quantity $ maxGas config
                        }
          chainId = 1 -- 1 is the mainnet chainId
          (sig1r, sig1s, sig1v) = decomposeSig sig1
          (sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
          issueCreditCall = issueCredit callVal
                                        ucac
                                        creditor debtor (UIntN amount)
                                        (sig1r :< sig1s :< sig1v :< NilL)
                                        (sig2r :< sig2s :< sig2v :< NilL)
                                        encodedMemo

      rawTx <- maybe (throwError (err500 {errBody = "Error generating txData."}))
                     pure $ createRawTransaction issueCreditCall
                                                 execNonce chainId
                                                 (executionPrivateKey config)
      result <- lndrWeb3 $ Eth.sendRawTransaction rawTx
      pure result


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
-- eth settlement amount.
verifySettlementPayment :: Maybe TransactionHash -> Address -> Address -> Integer -> LndrHandler ()
verifySettlementPayment (Just txnHash) creditorAddr debtorAddr settlementValue = do
    transactionM <- lndrWeb3 . Eth.getTransactionByHash $ addHexPrefix txnHash
    case transactionM of
        (Just transaction) ->
            let fromMatch = txFrom transaction == creditorAddr
                toMatch = txTo transaction == Just debtorAddr
                transactionValue = hexToInteger $ txValue transaction
                valueMatch = transactionValue == settlementValue
            in case (fromMatch, toMatch, valueMatch) of
                (False, _, _)      -> lndrError $ "Bad from match, hash: " ++ T.unpack txnHash
                (_, False, _)      -> lndrError $ "Bad to match, hash: " ++ T.unpack txnHash
                (_, _, False)      -> lndrError $ "Bad value match, hash: " ++ T.unpack txnHash
                                                 ++ "tx value: " ++ show transactionValue
                                                 ++ ", settlementValue: " ++ show settlementValue
                (True, True, True) -> pure ()
        Nothing -> lndrError $ "transaction not found, tx_hash: " ++ T.unpack txnHash
verifySettlementPayment _ _ _ _ = lndrError "Incompelete settlement record"


-- | Queries the blockchain for current blocknumber.
currentBlockNumber :: ServerConfig -> MaybeT IO Integer
currentBlockNumber config = do
    let provider = HttpProvider (web3Url config)
    blockNumberTextE <- runWeb3' provider Eth.blockNumber
    return $ case blockNumberTextE of
        Right (BlockNumber number) -> number
        Left _        -> 0


-- | Queries the blockchain for current nonce of the execution address
currentExecutionNonce :: ServerConfig -> MaybeT IO Integer
currentExecutionNonce config = do
    let provider = HttpProvider (web3Url config)
    nonceE <- runWeb3' provider $ Eth.getTransactionCount (executionAddress config) Latest
    return $ case nonceE of
        Right (Quantity number) -> number
        Left  _                 -> 0
