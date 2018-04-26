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
                                 , ("AUD", "Pending credit rejected by %s")
                                 , ("CAD", "Pending credit rejected by %s")
                                 , ("CHF", "Pending credit rejected by %s")
                                 , ("CNY", "Pending credit rejected by %s")
                                 , ("DKK", "Pending credit rejected by %s")
                                 , ("EUR", "Pending credit rejected by %s")
                                 , ("GBP", "Pending credit rejected by %s")
                                 , ("HKD", "Pending credit rejected by %s")
                                 , ("NOK", "Pending credit rejected by %s")
                                 , ("NZD", "Pending credit rejected by %s")
                                 , ("SEK", "Pending credit rejected by %s")
                                 , ("IDR", "Pending credit rejected by %s")
                                 , ("MYR", "Pending credit rejected by %s")
                                 , ("SGD", "Pending credit rejected by %s")
                                 , ("THB", "Pending credit rejected by %s")
                                 , ("VND", "Pending credit rejected by %s")
                                 , ("ILS", "Pending credit rejected by %s")
                                 , ("RUB", "Pending credit rejected by %s")
                                 , ("TRY", "Pending credit rejected by %s")
                                 , ("JPY", "%sにより拒否された、保留状態のクレジット")
                                 , ("KRW", "계류중거래 %s 께서 거절 했습니다") ]

pendingConfirmationMap :: M.Map String String
pendingConfirmationMap = M.fromList [ ("USD", "Pending credit confirmation by %s")
                                    , ("AUD", "Pending credit confirmation by %s")
                                    , ("CAD", "Pending credit confirmation by %s")
                                    , ("CHF", "Pending credit confirmation by %s")
                                    , ("CNY", "Pending credit confirmation by %s")
                                    , ("DKK", "Pending credit confirmation by %s")
                                    , ("EUR", "Pending credit confirmation by %s")
                                    , ("GBP", "Pending credit confirmation by %s")
                                    , ("HKD", "Pending credit confirmation by %s")
                                    , ("NOK", "Pending credit confirmation by %s")
                                    , ("NZD", "Pending credit confirmation by %s")
                                    , ("SEK", "Pending credit confirmation by %s")
                                    , ("IDR", "Pending credit confirmation by %s")
                                    , ("MYR", "Pending credit confirmation by %s")
                                    , ("SGD", "Pending credit confirmation by %s")
                                    , ("THB", "Pending credit confirmation by %s")
                                    , ("VND", "Pending credit confirmation by %s")
                                    , ("ILS", "Pending credit confirmation by %s")
                                    , ("RUB", "Pending credit confirmation by %s")
                                    , ("TRY", "Pending credit confirmation by %s")
                                    , ("JPY", "%sが承認した、保留状態のクレジット")
                                    , ("KRW", "계류중거래 %s 께서 승인 했습니다") ]

newPendingMap :: M.Map String String
newPendingMap = M.fromList [ ("USD", "New pending credit from %s")
                            , ("AUD", "New pending credit from %s")
                            , ("CAD", "New pending credit from %s")
                            , ("CHF", "New pending credit from %s")
                            , ("CNY", "New pending credit from %s")
                            , ("DKK", "New pending credit from %s")
                            , ("EUR", "New pending credit from %s")
                            , ("GBP", "New pending credit from %s")
                            , ("HKD", "New pending credit from %s")
                            , ("NOK", "New pending credit from %s")
                            , ("NZD", "New pending credit from %s")
                            , ("SEK", "New pending credit from %s")
                            , ("IDR", "New pending credit from %s")
                            , ("MYR", "New pending credit from %s")
                            , ("SGD", "New pending credit from %s")
                            , ("THB", "New pending credit from %s")
                            , ("VND", "New pending credit from %s")
                            , ("ILS", "New pending credit from %s")
                            , ("RUB", "New pending credit from %s")
                            , ("TRY", "New pending credit from %s")
                            , ("JPY", "%sからの保留になっている新しいクレジット")
                            , ("KRW", "새로운 계류중거래 %s 께서 추가 했습니다") ]
