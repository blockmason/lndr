{-# LANGUAGE OverloadedStrings #-}

module Lndr.IdentityVerification where

import qualified Data.Map            as M
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Char8   as BS
import qualified Data.Aeson as A
import           Lndr.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth, RequestBody(RequestBodyBS))
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types  as HTTP (hAccept)
import qualified Network.HTTP.Client.MultipartFormData as LM
import           System.Log.FastLogger

sendVerificationRequest :: ServerConfig -> IdentityVerificationRequest -> IO IdentityVerificationResponse
sendVerificationRequest config reqInfo = do
    initReq <- HTTP.parseRequest $ (sumsubApiUrl config) ++ "/resources/applicants?key=" ++ (sumsubApiKey config)
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.setRequestBodyJSON reqInfo $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseBody <$> HTTP.httpJSON req
    where acceptContent = "application/json"


sendVerificationDocument :: LoggerSet -> ServerConfig -> Text -> IdentityDocument -> IO IdentityDocument
sendVerificationDocument loggerSet config sumsubId idenDoc@(IdentityDocument idDocType idDocSubType country (Just file)) = do
    let content = B64.decodeLenient $ TE.encodeUtf8 file
        metadata = (VerificationMetaData idDocType idDocSubType country)
  
    -- pushLogStrLn loggerSet . toLogStr $ metadata

    initReq <- HTTP.parseRequest $ (sumsubApiUrl config) ++ "/resources/applicants/" ++ T.unpack sumsubId ++ "/info/idDoc?key=" ++ (sumsubApiKey config)
    bodyReq <- LM.formDataBody [ LM.partLBS "metadata" $ A.encode $ A.toJSON metadata
        , LM.partFileRequestBody "content" "image.jpg" $ HTTP.RequestBodyBS content ] initReq
    HTTP.getResponseBody <$> HTTP.httpJSON bodyReq


getVerificationStatus :: ServerConfig -> Text -> IO IdentityVerificationStatus
getVerificationStatus config id = do
    req <- HTTP.parseRequest $ (sumsubApiUrl config) ++ "/resources/applicants/" ++ T.unpack id ++ "/status/testCompleted?key=" ++ (sumsubApiKey config)
    res <- HTTP.getResponseBody <$> HTTP.httpJSON req :: IO IdentityVerificationStatus
    return res
