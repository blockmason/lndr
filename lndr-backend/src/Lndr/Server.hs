{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lndr.Server
    ( ServerState
    , LndrAPI(..)
    , lndrAPI
    , LndrHandler(..)
    , freshState
    , updateDbFromLndrLogs
    , app
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.Either (either)
import           Data.List ((\\))
import           Data.Pool (createPool, withResource)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Database.PostgreSQL.Simple as DB
import qualified Lndr.Db as Db
import           Lndr.Docs
import           Lndr.EthInterface
import           Lndr.Handler
import           Lndr.Types
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Address
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs


type LndrAPI =
        "transactions" :> QueryParam "user" Address :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> Capture "user" Address :> Get '[JSON] [CreditRecord]
   :<|> "lend" :> ReqBody '[JSON] CreditRecord :> PostNoContent '[JSON] NoContent
   :<|> "borrow" :> ReqBody '[JSON] CreditRecord :> PostNoContent '[JSON] NoContent
   :<|> "reject" :> ReqBody '[JSON] RejectRecord :> PostNoContent '[JSON] NoContent
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Nonce
   :<|> "nick" :> ReqBody '[JSON] NickRequest :> PostNoContent '[JSON] NoContent
   :<|> "nick" :> Capture "user" Address :> Get '[JSON] Text
   :<|> "search_nick" :> Capture "nick" Text :> Get '[JSON] [NickInfo]
   :<|> "friends" :> Capture "user" Address :> Get '[JSON] [NickInfo]
   :<|> "add_friends" :> Capture "user" Address
                      :> ReqBody '[JSON] [Address]
                      :> PostNoContent '[JSON] NoContent
   :<|> "remove_friends" :> Capture "user" Address
                         :> ReqBody '[JSON] [Address]
                         :> PostNoContent '[JSON] NoContent
   :<|> "counterparties" :> Capture "user" Address :> Get '[JSON] [Address]
   :<|> "balance" :> Capture "user" Address :> Get '[JSON] Integer
   :<|> "balance" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Integer
   :<|> "gas_price" :> Get '[JSON] Integer
   :<|> "gas_price" :> ReqBody '[JSON] Integer :> PutNoContent '[JSON] NoContent
   :<|> "docs" :> Raw


lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy

apiDocs :: API
apiDocs = docs lndrAPI

docsBS :: ByteString
docsBS = encodeUtf8
       . LT.pack
       . markdown
       $ docsWithIntros [intro] lndrAPI
  where intro = DocIntro "LNDR Server" ["Web service API"]

server :: ServerT LndrAPI LndrHandler
server = transactionsHandler
    :<|> pendingHandler
    :<|> lendHandler
    :<|> borrowHandler
    :<|> rejectHandler
    :<|> nonceHandler
    :<|> nickHandler
    :<|> nickLookupHandler
    :<|> nickSearchHandler
    :<|> friendHandler
    :<|> addFriendsHandler
    :<|> removeFriendsHandler
    :<|> counterpartiesHandler
    :<|> balanceHandler
    :<|> twoPartyBalanceHandler
    :<|> gasPriceHandler
    :<|> setGasPriceHandler
    :<|> Tagged serveDocs
    where serveDocs _ respond =
            respond $ responseLBS ok200 [plain] docsBS
          plain = ("Content-Type", "text/plain")


readerToHandler' :: forall a. ServerState -> LndrHandler a -> Handler a
readerToHandler' state r = do
    res <- liftIO . runExceptT $ runReaderT (runLndr r) state
    case res of
      Left err -> throwError err
      Right a  -> return a


readerToHandler :: ServerState -> LndrHandler :~> Handler
readerToHandler state = NT (readerToHandler' state)


readerServer :: ServerState -> Server LndrAPI
readerServer state = enter (readerToHandler state) server


app :: ServerState -> Application
app state = serve lndrAPI (readerServer state)


updateDbFromLndrLogs :: ServerState -> IO ()
updateDbFromLndrLogs (ServerState pool configMVar) = void $ do
    config <- takeMVar configMVar
    logs <- runWeb3 $ lndrLogs config Nothing Nothing
    withResource pool . Db.insertCredits $ either (const []) id logs


unsubmittedTransactions :: ServerState -> IO [IssueCreditLog]
unsubmittedTransactions (ServerState pool configMVar) = do
    config <- takeMVar configMVar
    blockchainCreditsE <- runWeb3 $ lndrLogs config Nothing Nothing
    let blockchainCredits = either (const []) id blockchainCreditsE
    dbCredits <- withResource pool Db.allCredits
    return $ dbCredits \\ blockchainCredits


freshState :: IO ServerState
freshState = do
    serverConfig <- loadConfig
    let dbConfig = DB.defaultConnectInfo { DB.connectUser = T.unpack $ dbUser serverConfig
                                         , DB.connectPassword = T.unpack $ dbUserPassword serverConfig
                                         , DB.connectDatabase = T.unpack $ dbName serverConfig
                                         }

    ServerState <$> createPool (DB.connect dbConfig) DB.close 1 10 95
                <*> newMVar serverConfig
