{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lndr.Handler.Types where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as B (fromStrict)
import           Data.Either.Combinators (mapLeft)
import           Lndr.Types
import           Network.Ethereum.Web3
import           Servant

newtype LndrHandler a = LndrHandler {
  runLndr :: ReaderT ServerState (ExceptT ServantErr IO) a
} deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError ServantErr, MonadIO)
-- TOOD rename this
web3ToLndr :: Show a => IO (Either a b) -> LndrHandler b
web3ToLndr = LndrHandler . lift . ExceptT . fmap (mapLeft (\x -> err500 { errBody = B.fromStrict . B.pack $ show x }))

lndrWeb3 :: Web3 DefaultProvider b -> LndrHandler b
lndrWeb3 = web3ToLndr . runWeb3

ioMaybeToLndr :: String -> IO (Maybe a) -> LndrHandler a
ioMaybeToLndr error = LndrHandler . lift . ExceptT . fmap (maybe (Left (err500 { errBody = B.fromStrict . B.pack $ error })) Right)
