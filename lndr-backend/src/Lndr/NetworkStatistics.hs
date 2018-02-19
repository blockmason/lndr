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


-- | Sets server's default gasPrice to the current safelow price on
-- ethgasstation.info. If a query to ethgasstation.info fails, the old value
-- for 'gasPrice' is used.
safelowUpdate :: ServerConfig -> TVar ServerConfig -> IO ServerConfig
safelowUpdate config configTVar = do
    req <- HTTP.parseRequest "https://ethgasstation.info/json/ethgasAPI.json"
    gasStationResponseE <- try (HTTP.getResponseBody <$> HTTP.httpJSON req)
    case gasStationResponseE of
        Right gasStationResponse -> do
            let lastestSafeLow = ceiling $ margin * safeLowScaling * safeLow gasStationResponse
                updatedConfg = config { gasPrice = lastestSafeLow }
            liftIO . atomically . modifyTVar configTVar $ const updatedConfg
            return updatedConfg
        Left (_ :: HTTP.HttpException) -> return config
    where
        safeLowScaling = 100000000 -- eth gas station returns prices in DeciGigaWei
        margin = 1.3 -- multiplier for  additional assurance that tx will make it into blockchain

-- | Queries the coinbase api and returns price in USD per 1 eth.
queryEtheruemPrice :: MaybeT IO EthereumPrice
queryEtheruemPrice = do
    req <- lift $ HTTP.parseRequest "https://api.coinbase.com/v2/exchange-rates?currency=ETH"
    MaybeT $ rightToMaybe . HTTP.getResponseBody <$> HTTP.httpJSONEither req


-- | Queries the blockchain for current blocknumber.
currentBlockNumber :: MaybeT IO Integer
currentBlockNumber = do
    blockNumberTextE <- runLndrWeb3 Eth.blockNumber
    return $ case blockNumberTextE of
        Right numText -> hexToInteger numText
        Left _        -> 0
