{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server ( ServerState
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
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Docs
import           EthInterface (freshState)
import           Handler
import           Network.Ethereum.Web3.Address
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs
import           Types

type LndrAPI =
        "transactions" :> Get '[JSON] [IssueCreditLog]
   :<|> "pending" :> Get '[JSON] [PendingRecord]
   :<|> "lend" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "borrow" :> ReqBody '[JSON] (CreditRecord Signed) :> PostNoContent '[JSON] NoContent
   :<|> "reject" :> ReqBody '[JSON] RejectRecord :> Post '[JSON] NoContent
   :<|> "nonce" :> Capture "p1" Address :> Capture "p2" Address :> Get '[JSON] Nonce
   :<|> "nick" :> ReqBody '[JSON] NickRequest :> PostNoContent '[JSON] NoContent
   :<|> "friends" :> Capture "user" Address :> Get '[JSON] [Address]
   :<|> "update_friends" :> Capture "user" Address
                         :> ReqBody '[JSON] UpdateFriendsRequest
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
    :<|> friendHandler
    :<|> updateFriendsHandler
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
