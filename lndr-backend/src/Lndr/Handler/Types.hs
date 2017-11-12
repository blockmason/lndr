{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lndr.Handler.Types where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Either.Combinators (mapLeft)
import           Lndr.Types
import           Network.Ethereum.Web3

newtype LndrHandler a = LndrHandler {
  runLndr :: ReaderT ServerState (ExceptT LndrError IO) a
} deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError LndrError, MonadIO)
-- TOOD rename this
web3ToLndr :: Show a => IO (Either a b) -> LndrHandler b
web3ToLndr = LndrHandler . lift . ExceptT . fmap (mapLeft (LndrError . show))


lndrWeb3 :: Web3 DefaultProvider b -> LndrHandler b
lndrWeb3 = web3ToLndr . runWeb3


ioMaybeToLndr :: String -> IO (Maybe a) -> LndrHandler a
ioMaybeToLndr error = LndrHandler . lift . ExceptT . fmap (maybe (Left (LndrError error)) Right)
