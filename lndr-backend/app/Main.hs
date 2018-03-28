module Main where

import           Lndr.Server
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Logger       (withStdoutLogger)

main :: IO ()
main = do
    emptyState <- freshState
    runHeartbeat emptyState
    serverConfig <- serverConfig emptyState
    withStdoutLogger $ \aplogger -> do
        let address = bindAddress serverConfig
        let port = bindPort serverConfig
        let settings = W.setHost address $ W.setPort port $ W.setLogger aplogger W.defaultSettings
        W.runSettings settings $ app emptyState
