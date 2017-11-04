{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import qualified Network.Ethereum.Web3.Address as Address
import           Servant

import           Server

readerToHandler' :: forall a. ServerState -> ReaderT ServerState IO a -> Handler a
readerToHandler' state r = liftIO (runReaderT r state)

readerToHandler :: ServerState -> ReaderT ServerState IO :~> Handler
readerToHandler state = NT (readerToHandler' state)

readerServer :: ServerState -> Server FiddyAPI
readerServer state = enter (readerToHandler state) server


app :: ServerState -> Application
app state = serve fiddyAPI (readerServer state)

main :: IO ()
main = do
    pendingMap <- freshState
    N.run 80 $ app pendingMap
