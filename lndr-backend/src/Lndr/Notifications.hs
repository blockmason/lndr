{-# LANGUAGE OverloadedStrings #-}

module Lndr.Notifications where

import qualified Data.Map            as M
import           Data.Text           (Text)
import           Lndr.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types  as HTTP (hAccept)


-- | Sends a request to the Urban Airship api to send push notifications to
-- specific mobile devices.
sendNotification :: ServerConfig -> Notification -> IO Int
sendNotification config notification = do
    initReq <- HTTP.parseRequest urbanAirshipUrl
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.applyBasicAuth (urbanAirshipKey config) (urbanAirshipSecret config) $
                    HTTP.setRequestBodyJSON notification $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    where acceptContent = "application/vnd.urbanairship+json; version=3;"
          urbanAirshipUrl = "https://go.urbanairship.com/api/push"

notificationMessages = "Pending credit confirmation from "

pendingRejectionMap :: M.Map String String
pendingRejectionMap = M.fromList [ ("USD", "Pending credit rejected by %s")
                                 , ("JPY", "%sにより拒否された、保留状態のクレジット")
                                 , ("KRW", "계류중거래 %s 께서 거절 했습니다") ]

pendingConfirmationMap :: M.Map String String
pendingConfirmationMap = M.fromList [ ("USD", "Pending credit confirmation by %s")
                                    , ("JPY", "%sが承認した、保留状態のクレジット")
                                    , ("KRW", "계류중거래 %s 께서 승인 했습니다") ]

newPendingMap :: M.Map String String
newPendingMap = M.fromList [ ("USD", "New pending credit from %s")
                           , ("JPY", "%sからの保留になっている新しいクレジット")
                           , ("KRW", "새로운 계류중거래 %s 께서 추가 했습니다") ]
