{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lndr.Server
    ( ServerState
    , LndrAPI(..)
    , lndrAPI
    , LndrError(..)
    , LndrHandler(..)
    , freshState
    , app
    ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Lndr.Docs
import           Lndr.EthInterface (freshState)
import           Lndr.Handler
import           Lndr.Types
import           Network.Ethereum.Web3.Address
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs

type LndrAPI =
        "transactions" :> QueryParam "user" Address :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> QueryParam "user" Address :> Get '[JSON] [PendingRecord]
   :<|> "lend" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "borrow" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "reject" :> ReqBody '[JSON] RejectRecord :> Post '[JSON] NoContent
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Nonce
   :<|> "nick" :> ReqBody '[JSON] NickRequest :> PostNoContent '[JSON] NoContent
   :<|> "nick" :> Capture "user" Address :> Get '[JSON] Text
   :<|> "friends" :> Capture "user" Address :> Get '[JSON] [Address]
   :<|> "add_friends" :> Capture "user" Address
                      :> ReqBody '[JSON] [Address]
                      :> PostNoContent '[JSON] NoContent
   :<|> "remove_friends" :> Capture "user" Address
                         :> ReqBody '[JSON] [Address]
                         :> PostNoContent '[JSON] NoContent
   :<|> "docs" :> Raw


lndrAPI :: Proxy LndrAPI
lndrAPI = Proxy

apiDocs :: API
apiDocs = docs lndrAPI

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
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
    :<|> friendHandler
    :<|> addFriendsHandler
    :<|> removeFriendsHandler
    :<|> Tagged serveDocs
    where serveDocs _ respond =
            respond $ responseLBS ok200 [plain] docsBS
          plain = ("Content-Type", "text/plain")

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
