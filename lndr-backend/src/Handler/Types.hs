{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.Types where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Either.Combinators (mapLeft)
import           Network.Ethereum.Web3
import           Types

newtype LndrHandler a = LndrHandler {
  runLndr :: ReaderT ServerState (ExceptT LndrError IO) a
} deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError LndrError, MonadIO)

web3ToLndr :: Show a => IO (Either a b) -> LndrHandler b
web3ToLndr = LndrHandler . lift . ExceptT . fmap (mapLeft (LndrError . show))


lndrWeb3 :: Web3 DefaultProvider b -> LndrHandler b
lndrWeb3 = web3ToLndr . runWeb3
