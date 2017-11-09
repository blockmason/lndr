{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Either
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Logger (withStdoutLogger)
import qualified Network.Ethereum.Web3.Address as Address
import           Servant

import           EthInterface
import           Server
import           Docs

readerToHandler' :: forall a. ServerState -> LndrHandler a -> Handler a
readerToHandler' state r = do
    res <- liftIO . runExceptT $ runReaderT (runLndr r) state
    Handler . ExceptT . return $ case res of
      Left (LndrError text) -> Left err500 { errBody = T.encodeUtf8 $ T.pack text }
      Right a  -> Right a

readerToHandler :: ServerState -> LndrHandler :~> Handler
readerToHandler state = NT (readerToHandler' state)


readerServer :: ServerState -> Server LndrAPI
readerServer state = enter (readerToHandler state) server


app :: ServerState -> Application
app state = serve lndrAPI (readerServer state)


server :: ServerT LndrAPI LndrHandler
server = transactionsHandler
    :<|> pendingHandler
    :<|> lendHandler
    :<|> borrowHandler
    :<|> rejectHandler
    :<|> nonceHandler
    :<|> nickHandler
    :<|> friendHandler
    :<|> Tagged serveDocs
    where serveDocs _ respond =
            respond $ responseLBS ok200 [plain] docsBS
          plain = ("Content-Type", "text/plain")


main :: IO ()
main = do
    emptyState <- freshState
    withStdoutLogger $ \aplogger -> do
        let settings = W.setPort 80 $ W.setLogger aplogger W.defaultSettings
        W.runSettings settings $ app emptyState
