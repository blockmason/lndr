{-# LANGUAGE ScopedTypeVariables #-}

module Lndr.NetworkStatistics where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Either.Combinators
import           Lndr.Types
import           Lndr.Util
import           Lndr.Web3
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth as Eth
import qualified Network.HTTP.Simple       as HTTP

-- | Returns the current safelow price on ethgasstation.info.
querySafelow :: MaybeT IO Integer
querySafelow = do
    req <- lift $ HTTP.parseRequest "https://ethgasstation.info/json/ethgasAPI.json"
    gasStationResponse <- MaybeT $ rightToMaybe . HTTP.getResponseBody <$> HTTP.httpJSONEither req
    return . ceiling $ margin * safeLowScaling * safeLow gasStationResponse
    where
        safeLowScaling = 100000000 -- eth gas station returns prices in DeciGigaWei
        margin = 1.3 -- multiplier for additional assurance that tx will make it into blockchain

-- | Queries the coinbase api and returns price in USD per 1 eth.
queryEtheruemPrices :: MaybeT IO EthereumPrices
queryEtheruemPrices = do
    req <- lift $ HTTP.parseRequest "https://api.coinbase.com/v2/exchange-rates?currency=ETH"
    MaybeT $ rightToMaybe . HTTP.getResponseBody <$> HTTP.httpJSONEither req


-- | Queries the blockchain for current blocknumber.
currentBlockNumber :: MaybeT IO Integer
currentBlockNumber = do
    blockNumberTextE <- runLndrWeb3 Eth.blockNumber
    return $ case blockNumberTextE of
        Right numText -> hexToInteger numText
        Left _        -> 0
