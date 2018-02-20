{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lndr.Docs where

import           Data.Maybe                    (fromJust)
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import           Lndr.Types
import           Network.Ethereum.Web3.Address
import           Servant.API
import           Servant.Docs
import           Text.EmailAddress             as Email

creditHash :: Text
creditHash = "0x7e2e9ff3a5fc148cf76261755c4c666630bfc3a28d02733cfbe721fc965aca28"

crSigned :: CreditRecord
crSigned = CreditRecord "0x11edd217a875063583dd1b638d16810c5d34d54b"
                        "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                        69
                        "test memo"
                        "0x11edd217a875063583dd1b638d16810c5d34d54b"
                        0
                        "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"
                        "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"
                        "0x718d7217a875063583dd1b638d16810c5d34d54b"
                        Nothing
                        Nothing
                        Nothing

crSettleSigned :: SettlementCreditRecord
crSettleSigned = SettlementCreditRecord crSigned (Just "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c")

instance ToSample ConfigResponse where
    toSamples _ = singleSample $
        ConfigResponse (M.fromList [("USD", "0x7899b83071d9704af0b132859a04bb1698a3acaf")])
                       "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"

instance ToSample SettlementsResponse where
    toSamples _ = singleSample $ SettlementsResponse [crSigned] [crSettleSigned]


instance ToSample CreditRecord where
    toSamples _ = singleSample crSigned

instance ToSample RejectRequest where
    toSamples _ = singleSample $
        RejectRequest "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"
                      "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"

instance ToSample NickRequest where
    toSamples _ = singleSample $
        NickRequest "0x11edd217a875063583dd1b638d16810c5d34d54b" "aupiff" "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"

instance ToSample EmailAddress where
    toSamples _ = singleSample $
        fromJust $ Email.emailAddressFromText "tim@blockmason.io"

instance ToSample EmailRequest where
    toSamples _ = singleSample $
        EmailRequest "0x11edd217a875063583dd1b638d16810c5d34d54b" (fromJust $ Email.emailAddressFromText "tim@blockmason.io") "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"

instance ToSample NickInfo where
    toSamples _ = singleSample $
        NickInfo "0x11edd217a875063583dd1b638d16810c5d34d54b" "aupiff"

instance ToSample IssueCreditLog where
    toSamples _ = singleSample $
        IssueCreditLog "d5ec73eac35fc9dd6c3f440bce314779fed09f60"
                       "0x11edd217a875063583dd1b638d16810c5d34d54b"
                       "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                       69
                       0
                       "simple memo"

instance ToSample Address where
    toSamples _ = singleSample "0x11edd217a875063583dd1b638d16810c5d34d54b"

instance ToSample VerifySettlementRequest where
    toSamples _ = singleSample $ VerifySettlementRequest "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"
                                                         "0xf357c689de57464713697787d4c40a78feda913162911e191e545343ff769999"
                                                         "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                                                         "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"


instance ToSample PushRequest where
    toSamples _ = singleSample $ PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios"
                                             "0x11edd217a875063583dd1b638d16810c5d34d54b" ""


instance ToSample ProfilePhotoRequest where
    toSamples _ = singleSample $ ProfilePhotoRequest "2394" "239048"


instance ToSample Text where
    toSamples _ = singleSample "aupiff"

instance ToSample Nonce where
    toSamples _ = singleSample $ Nonce 1

instance ToSample Integer where
    toSamples _ = singleSample 19

instance ToCapture (Capture "email" EmailAddress) where
  toCapture _ =
    DocCapture "email" "the email whose availability is being checked"

instance ToCapture (Capture "p1" Address) where
  toCapture _ =
    DocCapture "p1" "the address of the first party in a credit relationship"

instance ToCapture (Capture "p2" Address) where
  toCapture _ =
    DocCapture "p2" "the address of the second party in a credit relationship"

instance ToCapture (Capture "address" Address) where
  toCapture _ =
    DocCapture "address" "the address to which a nickname should be assigned"

instance ToCapture (Capture "user" Address) where
  toCapture _ =
    DocCapture "user" "the address of the user whose friends will be returned"

instance ToCapture (Capture "nick" Text) where
  toCapture _ =
    DocCapture "nick" "the nickname to be associated with a particular address"

instance ToCapture (Capture "email" Text) where
  toCapture _ =
    DocCapture "email" "the email to be associated with a particular address"

instance ToCapture (Capture "hash" Text) where
  toCapture _ =
    DocCapture "hash" "the hash by which to identify a credit record among candidates for resubmission"

instance ToParam (QueryParam "user" Address) where
  toParam _ =
    DocQueryParam "user"
                  [ "0x11edd217a875063583dd1b638d16810c5d34d54b"
                  , "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb" ]
                  "address of user whose records to display"
                  Normal

instance ToParam (QueryParam "txHash" Text) where
  toParam _ =
    DocQueryParam "txHash"
                  [ "0x3cb5a3890ca592a6e857f83413bef59dcc59c25445922dbcb3c5623563301639"
                  , "0x5c0dede3096ec95edaadc2cd258514ee837c45a55a71b05273837c123947ba0d" ]
                  "address of user whose records to display"
                  Normal

instance ToParam (QueryParam "email" EmailAddress) where
  toParam _ =
    DocQueryParam "email"
                  [ "tim@blockmason.io"
                  , "michael@blockmason.io" ]
                  "email whose corresponding user address is requested"
                  Normal

instance ToParam (QueryParam "nick" Nick) where
  toParam _ =
    DocQueryParam "nick"
                  [ "aupiff"
                  , "willbach" ]
                  "nickname whose corresponding user address is requested"
                  Normal
