{-# LANGUAGE OverloadedStrings #-}

module Lndr.IdentityVerification where

import qualified Data.Map            as M
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Lndr.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types  as HTTP (hAccept)


sendVerificationRequest :: ServerConfig -> IdentityVerificationRequest -> IO IdentityVerificationResponse
sendVerificationRequest config reqInfo = do
    initReq <- HTTP.parseRequest $ (sumsubApiUrl config) ++ "/resources/applicants?key=" ++ (sumsubApiKey config)
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.setRequestBodyJSON reqInfo $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseBody <$> HTTP.httpJSON req
    where acceptContent = "application/json"


getVerificationStatus :: ServerConfig -> Text -> IO IdentityVerificationStatus
getVerificationStatus config id = do
    req <- HTTP.parseRequest $ (sumsubApiUrl config) ++ "/resources/applicants/" ++ T.unpack id ++ "/status/testCompleted?key=" ++ (sumsubApiKey config)
    res <- HTTP.getResponseBody <$> HTTP.httpJSON req :: IO IdentityVerificationStatus
    return res
