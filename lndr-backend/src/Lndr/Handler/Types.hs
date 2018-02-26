{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lndr.Handler.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as B (pack)
import qualified Data.ByteString.Lazy       as B (fromStrict)
import           Data.Either.Combinators    (mapLeft)
import           Lndr.Types
import           Lndr.Web3
import           Network.Ethereum.Web3
import           Servant

newtype LndrHandler a = LndrHandler {
  runLndr :: ReaderT ServerState (ExceptT ServantErr IO) a
} deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError ServantErr, MonadIO)

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return


lndrWeb3 :: Web3 LndrProvider b -> LndrHandler b
lndrWeb3 = ioEitherToLndr . runLndrWeb3


eitherToLndr :: Show a => String -> Either a b -> LndrHandler b
eitherToLndr error = liftEither . mapLeft (const (err400 { errBody = B.fromStrict . B.pack $ error }))


ioEitherToLndr :: Show a => IO (Either a b) -> LndrHandler b
ioEitherToLndr = LndrHandler . lift . ExceptT . fmap (mapLeft (\x -> err500 { errBody = B.fromStrict . B.pack $ show x }))


ioMaybeToLndr :: String -> IO (Maybe a) -> LndrHandler a
ioMaybeToLndr error = LndrHandler . lift . ExceptT . fmap (maybe (Left (err404 { errBody = B.fromStrict . B.pack $ error })) Right)
