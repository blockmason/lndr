{-# LANGUAGE OverloadedStrings #-}

module Lndr.Notifications where

import           Lndr.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP (hAccept)

-- | Sends a request to the Urban Airship api to send push notifications to
-- specific mobile devices.
sendNotification :: ServerConfig -> Notification -> IO Int
sendNotification config notification = do
    initReq <- HTTP.parseRequest urbanAirshipUrl
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.applyBasicAuth (urbanAirshipKey config) (urbanAirshipSecret config) $
                    HTTP.setRequestBodyJSON notification $ HTTP.setRequestMethod "POST" initReq
    httpCode <- HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    putStrLn $ "notification code" ++ show httpCode
    print notification
    return httpCode
    where acceptContent = "application/vnd.urbanairship+json; version=3;"
          urbanAirshipUrl = "https://go.urbanairship.com/api/push"
