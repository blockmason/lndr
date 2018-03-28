{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module Lndr.Server
    ( ServerState
    , LndrAPI
    , lndrAPI
    , LndrHandler(..)
    , freshState
    , app
    , runHeartbeat
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception          (SomeException, catch)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Either                (either)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (createPool, withResource)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import           Lndr.Config
import qualified Lndr.Db                    as Db
import           Lndr.Docs
import           Lndr.EthereumInterface
import           Lndr.Handler
import           Lndr.NetworkStatistics
import           Lndr.Types
import           Network.Ethereum.Web3      hiding (convert)
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs
import           System.Log.FastLogger
import           Text.EmailAddress

type LndrAPI =
        "transactions" :> QueryParam "user" Address :> Get '[JSON] [IssueCreditLog]
   :<|> "pending_settlements" :> Capture "user" Address :> Get '[JSON] SettlementsResponse
   :<|> "verify_settlement" :> ReqBody '[JSON] VerifySettlementRequest
                            :> PostNoContent '[JSON] NoContent
   :<|> "tx_hash" :> Capture "hash" Text :> Get '[JSON] Text
   :<|> "pending" :> Capture "user" Address :> Get '[JSON] [CreditRecord]
   :<|> "lend" :> ReqBody '[JSON] CreditRecord :> PostNoContent '[JSON] NoContent
   :<|> "borrow" :> ReqBody '[JSON] CreditRecord :> PostNoContent '[JSON] NoContent
   :<|> "reject" :> ReqBody '[JSON] RejectRequest :> PostNoContent '[JSON] NoContent
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Nonce
   :<|> "nick" :> ReqBody '[JSON] NickRequest :> PostNoContent '[JSON] NoContent
   :<|> "nick" :> Capture "user" Address :> Get '[JSON] Text
   :<|> "search_nick" :> Capture "nick" Text :> Get '[JSON] [UserInfo]
   :<|> "email" :> ReqBody '[JSON] EmailRequest :> PostNoContent '[JSON] NoContent
   :<|> "email" :> Capture "user" Address :> Get '[JSON] EmailAddress
   :<|> "profile_photo" :> ReqBody '[JSON] ProfilePhotoRequest
                        :> PostNoContent '[JSON] NoContent
   :<|> "user" :> QueryParam "email" EmailAddress
               :> QueryParam "nick" Nick
               :> Get '[JSON] UserInfo
   :<|> "friends" :> Capture "user" Address :> Get '[JSON] [UserInfo]
   :<|> "friend_requests" :> Capture "user" Address :> Get '[JSON] [UserInfo]
   :<|> "add_friends" :> Capture "user" Address
                      :> ReqBody '[JSON] [Address]
                      :> PostNoContent '[JSON] NoContent
   :<|> "remove_friends" :> Capture "user" Address
                         :> ReqBody '[JSON] [Address]
                         :> PostNoContent '[JSON] NoContent
   :<|> "counterparties" :> Capture "user" Address :> Get '[JSON] [Address]
   :<|> "balance" :> Capture "user" Address :> QueryParam "currency" Text
                  :> Get '[JSON] Integer
   :<|> "balance" :> Capture "p1" Address :> Capture "p2" Address
                  :> QueryParam "currency" Text :> Get '[JSON] Integer
   :<|> "unsubmitted" :> Get '[JSON] (Int, Int, [IssueCreditLog])
   :<|> "register_push" :> ReqBody '[JSON] PushRequest
                        :> PostNoContent '[JSON] NoContent
   :<|> "config" :> Get '[JSON] ConfigResponse
   :<|> "docs" :> Raw


server :: ServerT LndrAPI LndrHandler
server = transactionsHandler
    :<|> pendingSettlementsHandler
    :<|> verifyHandler
    :<|> txHashHandler
    :<|> pendingHandler
    :<|> lendHandler
    :<|> borrowHandler
    :<|> rejectHandler
    :<|> nonceHandler
    :<|> nickHandler
    :<|> nickLookupHandler
    :<|> nickSearchHandler
    :<|> emailHandler
    :<|> emailLookupHandler
    :<|> photoUploadHandler
    :<|> userHandler
    :<|> friendHandler
    :<|> friendRequestsHandler
    :<|> addFriendsHandler
    :<|> removeFriendsHandler
    :<|> counterpartiesHandler
    :<|> balanceHandler
    :<|> twoPartyBalanceHandler
    :<|> unsubmittedHandler
    :<|> registerPushHandler
    :<|> configHandler
    :<|> Tagged serveDocs
    where serveDocs _ respond =
            respond $ responseLBS ok200 [plain] docsBS
          plain = ("Content-Type", "text/plain")


lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy


docsBS :: ByteString
docsBS = encodeUtf8
       . LT.pack
       . markdown
       $ docsWithIntros [intro] lndrAPI
  where intro = DocIntro "LNDR Server" ["Web service API"]


-- Natural Transformation from 'LndrHandler' to 'Handler'. Servant expects all
-- routes to be of type 'ExceptT ServantErr IO a' ('Handler') so the endpoints
-- used in this application, of type 'ReaderT ServerState (ExceptT ServantErr IO)'
-- ('LndrHandler'), must be converted to the default 'Handler' type before they
-- can be served by the 'serve' function.
lndrHandlerToHandler :: ServerState -> LndrHandler :~> Handler
lndrHandlerToHandler state = NT (lndrHandlerToHandler' state)
    where lndrHandlerToHandler' :: forall a. ServerState -> LndrHandler a -> Handler a
          lndrHandlerToHandler' state r = do
                res <- liftIO . runExceptT $ runReaderT (runLndr r) state
                case res of
                  Left err -> throwError err
                  Right a  -> return a


readerServer :: ServerState -> Server LndrAPI
readerServer state = enter (lndrHandlerToHandler state) server


app :: ServerState -> Application
app state = serve lndrAPI (readerServer state)


-- | Load required server configuration and create database connection pool.
-- Called at server startup.
freshState :: IO ServerState
freshState = do
    serverConfig <- loadConfig
    setEnvironmentConfigs serverConfig
    let dbConfig = DB.defaultConnectInfo {
          DB.connectHost = dbHost serverConfig
        , DB.connectPort = dbPort serverConfig
        , DB.connectUser = T.unpack $ dbUser serverConfig
        , DB.connectPassword = T.unpack $ dbUserPassword serverConfig
        , DB.connectDatabase = T.unpack $ dbName serverConfig
        }
        subpools = 1
        unusedKeepAliveSeconds = 10
        maximumResourcesPerSubpool = 95
    ServerState <$> createPool (DB.connect dbConfig) DB.close
                               subpools
                               unusedKeepAliveSeconds
                               maximumResourcesPerSubpool
                <*> newTVarIO serverConfig
                <*> newStdoutLoggerSet defaultBufSize


runHeartbeat :: ServerState -> IO ThreadId
runHeartbeat state = forkIO . forever $ catch (void . runExceptT $ runReaderT (runLndr heartbeat) state) (print :: SomeException -> IO ())


heartbeat :: LndrHandler ()
heartbeat = do
    (ServerState _ configTVar loggerSet) <- ask
    -- update server config
    liftIO $ updateServerConfig configTVar
    -- scan settlements table for any settlement eligible for deletion
    deleteExpiredSettlements
    -- try to verify all settlements whose tx_hash column is populated
    verifySettlementsWithTxHash
    -- log hearbeat statistics
    liftIO $ pushLogStrLn loggerSet . toLogStr $ ("heartbeat" :: Text)
    -- sleep for time specified in config
    config <- liftIO . atomically $ readTVar configTVar
    liftIO $ threadDelay (heartbeatInterval config * 10 ^ 6)


updateServerConfig :: TVar ServerConfig -> IO ()
updateServerConfig configTVar = do
    config <- atomically $ readTVar configTVar
    currentPricesM <- runMaybeT queryEtheruemPrices
    currentGasPriceM <- runMaybeT querySafelow
    blockNumberM <- runMaybeT $ currentBlockNumber config
    atomically $ writeTVar configTVar
        config { ethereumPrices = fromMaybe (ethereumPrices config) currentPricesM
               , gasPrice = fromMaybe (gasPrice config) currentGasPriceM
               , latestBlockNumber = fromMaybe (latestBlockNumber config) blockNumberM }


deleteExpiredSettlements :: LndrHandler ()
deleteExpiredSettlements = do
    (ServerState pool configTVar _) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    void . liftIO $ withResource pool Db.deleteExpiredSettlementsAndAssociatedCredits


verifySettlementsWithTxHash :: LndrHandler ()
verifySettlementsWithTxHash = do
    (ServerState pool configTVar _) <- ask
    config <- liftIO . atomically $ readTVar configTVar
    creditHashes <- liftIO $ withResource pool Db.settlementCreditsToVerify
    mapM_ verifyIndividualRecord creditHashes


verifyIndividualRecord :: TransactionHash -> LndrHandler ()
verifyIndividualRecord creditHash = do
    (ServerState pool configTVar loggerSet) <- ask
    config <- liftIO $ atomically $ readTVar configTVar
    recordM <- liftIO $ withResource pool $ Db.lookupCreditByHash creditHash
    let recordNotFound = throwError $
            err404 { errBody = "Credit hash does not refer to pending bilateral settlement record" }
    bilateralCreditRecord <- maybe recordNotFound pure recordM
    verifySettlementPayment bilateralCreditRecord
    liftIO $ withResource pool $ Db.verifyCreditByHash creditHash
    web3Result <- finalizeTransaction config bilateralCreditRecord
    liftIO $ pushLogStrLn loggerSet . toLogStr . ("WEB3: " ++) . show $ web3Result
