{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy (pack)
import           Network.HTTP.Types
import           Network.Wai
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Server
import           EthInterface

creditHash :: Text
creditHash = "0x7e2e9ff3a5fc148cf76261755c4c666630bfc3a28d02733cfbe721fc965aca28"

crSigned :: CreditRecord Signed
crSigned = CreditRecord "0x11edd217a875063583dd1b638d16810c5d34d54b"
                        "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                        69
                        "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"

cr :: CreditRecord Unsigned
cr = CreditRecord "0x11edd217a875063583dd1b638d16810c5d34d54b"
                 "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                 69
                 "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"

instance ToSample Text where
    toSamples _ = singleSample creditHash

instance ToSample (CreditRecord Signed) where
    toSamples _ = singleSample crSigned


instance ToSample (CreditRecord Unsigned) where
    toSamples _ = singleSample cr

instance ToSample IssueCreditLog where
    toSamples _ = singleSample $
        IssueCreditLog "d5ec73eac35fc9dd6c3f440bce314779fed09f60"
                       "0x11edd217a875063583dd1b638d16810c5d34d54b"
                       "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                       69

instance ToSample SubmissionResponse where
    toSamples _ = singleSample $ SubmissionResponse "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c" 1

apiDocs :: API
apiDocs = docs fiddyAPI
