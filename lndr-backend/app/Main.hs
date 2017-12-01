module Main where

import           Lndr.Server
import           Lndr.EthInterface
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Logger (withStdoutLogger)

main :: IO ()
main = do
    emptyState <- freshState
    syncTransactions emptyState
    withStdoutLogger $ \aplogger -> do
        let settings = W.setPort 80 $ W.setLogger aplogger W.defaultSettings
        W.runSettings settings $ app emptyState
