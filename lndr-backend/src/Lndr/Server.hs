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
    , updateDbFromLndrLogs
    , app
    , runHeartbeat
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.ByteString.Lazy       (ByteString)
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
import           Lndr.Web3
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


-- | Scans blockchain for previously-submitted credit records and inserts them
-- into 'verified_credits' table if missing.
--
-- This function is called at startup to ensure database consistency with the
-- blockchain. All credit-related queries use database data so without these
-- consistency checks at startup, it's possible user's transaction history
-- would be inaccurately represented.
--
-- When credits are recovered from blockchain, we lose 'submitter' information
-- so 'submitter' is set to be equal to 'creditor'
updateDbFromLndrLogs :: ServerState -> IO ()
updateDbFromLndrLogs (ServerState pool configMVar _) = void $ do
    config <- atomically $ readTVar configMVar
    logs <- runLndrWeb3 $ join <$> sequence [ lndrLogs config "USD" Nothing Nothing
                                            , lndrLogs config "JPY" Nothing Nothing
                                            , lndrLogs config "KRW" Nothing Nothing ]
    withResource pool . Db.insertCredits $ either (const []) id logs


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
    ServerState <$> createPool (DB.connect dbConfig) DB.close 1 10 95
                <*> newTVarIO serverConfig
                <*> newStdoutLoggerSet defaultBufSize


runHeartbeat :: ServerState -> IO ThreadId
runHeartbeat state = forkIO . forever $ heartbeat state


heartbeat :: ServerState -> IO ()
heartbeat state@(ServerState _ configMVar loggerSet) = do
    -- update server config
    updateServerConfig configMVar
    -- scan settlements table for any settlement eligible for deletion
    deleteExpiredSettlements state
    -- try to verify all settlements whose tx_hash column is populated
    verifySettlementsWithTxHash state
    -- log hearbeat statistics
    pushLogStrLn loggerSet . toLogStr $ ("heartbeat" :: Text)
    -- sleep for time specified in config
    config <- atomically $ readTVar configMVar
    threadDelay (heartbeatInterval config * 10 ^ 6)


updateServerConfig :: TVar ServerConfig -> IO ()
updateServerConfig configTVar = do
    config <- atomically $ readTVar configTVar
    currentPricesM <- runMaybeT queryEtheruemPrices
    currentGasPriceM <- runMaybeT querySafelow
    blockNumberM <- runMaybeT currentBlockNumber
    atomically $ writeTVar configTVar
        config { ethereumPrices = fromMaybe (ethereumPrices config) currentPricesM
               , gasPrice = fromMaybe (gasPrice config) currentGasPriceM
               , latestBlockNumber = fromMaybe (latestBlockNumber config) blockNumberM }


deleteExpiredSettlements :: ServerState -> IO ()
deleteExpiredSettlements (ServerState pool configMVar _) = do
    config <- atomically $ readTVar configMVar
    void $ withResource pool Db.deleteExpiredSettlementsAndAssociatedCredits


verifySettlementsWithTxHash :: ServerState -> IO ()
verifySettlementsWithTxHash state@(ServerState pool configMVar _) = do
    config <- atomically $ readTVar configMVar
    creditHashes <- withResource pool Db.settlementCreditsToVerify
    mapM_ (runExceptT . verifyIndividualRecord state) creditHashes
    return ()


verifyIndividualRecord :: ServerState -> TransactionHash -> ExceptT ServantErr IO ()
verifyIndividualRecord (ServerState pool configTVar loggerSet) creditHash = do
    config <- liftIO $ atomically $ readTVar configTVar
    recordM <- liftIO $ withResource pool $ Db.lookupCreditByHash creditHash
    let recordNotFound = throwError $
            err404 { errBody = "Credit hash does not refer to pending bilateral settlement record" }
    bilateralCreditRecord <- maybe recordNotFound pure recordM
    verified <- liftIO $ verifySettlementPayment bilateralCreditRecord
    if verified
        -- TODO unify it with other finalizeTransaction
        then do liftIO $ withResource pool $ Db.verifyCreditByHash creditHash
                web3Result <- liftIO $ finalizeTransaction config bilateralCreditRecord
                liftIO $ pushLogStrLn loggerSet . toLogStr . ("WEB3: " ++) . show $ web3Result
        else throwError $ err400 { errBody = "Unable to verify debt settlement" }
