module Main where

import           Lndr.Server
import           Lndr.Types
import           Lndr.Util
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Logger       (withStdoutLogger)

main :: IO ()
main = do
    emptyState <- freshState
    runHeartbeat emptyState
    serverConf <- currentConfig emptyState
    withStdoutLogger $ \aplogger -> do
        let address = textToHostPreference $ bindAddress serverConf
            port = bindPort serverConf
            settings = W.setHost address . W.setPort port $
                        W.setLogger aplogger W.defaultSettings
        W.runSettings settings $ app emptyState
