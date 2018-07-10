module Lndr.Signature where

import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lndr.Types
import           Lndr.Util
import qualified Network.Ethereum.Util         as EU
import           Network.Ethereum.Web3.Address
import qualified Network.Ethereum.Web3.Address as Addr
import           Text.EmailAddress as Email

class VerifiableSignature a where
     recoverSigner :: a -> Either String Address
     recoverSigner x = fmap textToAddress . EU.ecrecover (extractSignature x)
                                          . generateHash $ x

     extractSignature :: a -> Text

     generateHash :: a -> Text

     generateSignature :: a -> Text -> Either String Text
     generateSignature request = EU.ecsign (generateHash request)


instance VerifiableSignature CreditRecord where
    -- TODO make this more readable after transition to lens
    extractSignature = signature

    generateHash (CreditRecord creditor debtor amount _ _ nonce _ _ ucac _ _ _) =
        EU.hashText . T.concat $ stripHexPrefix <$> [ Addr.toText ucac
                                                    , Addr.toText creditor
                                                    , Addr.toText debtor
                                                    , integerToHex amount
                                                    , integerToHex nonce
                                                    ]


instance VerifiableSignature NickRequest where
    extractSignature (NickRequest _ _ sig) = sig

    generateHash (NickRequest addr nick _) = EU.hashText . T.concat $
            stripHexPrefix <$> [ T.pack (show addr) , bytesEncode nick ]

instance VerifiableSignature EmailRequest where
    extractSignature (EmailRequest _ _ sig) = sig

    generateHash (EmailRequest addr email _) =
        EU.hashText . T.concat $
            stripHexPrefix <$> [ T.pack (show addr)
                               , bytesEncode $ Email.toText email
                               ]


instance VerifiableSignature ProfilePhotoRequest where
    extractSignature (ProfilePhotoRequest _ sig) = sig

    generateHash (ProfilePhotoRequest image _) = EU.hashText . bytesEncode $ image


instance VerifiableSignature VerifySettlementRequest where
    extractSignature (VerifySettlementRequest _ _ _ sig) = sig

    generateHash (VerifySettlementRequest creditHash txHash creditorAddress _) =
        EU.hashText . T.concat $
            stripHexPrefix <$> [ creditHash
                               , txHash
                               , T.pack (show creditorAddress) ]

instance VerifiableSignature PushRequest where
    extractSignature (PushRequest _ _ _ sig) = sig

    generateHash (PushRequest channelID platform addr _) = EU.hashText . T.concat $
        stripHexPrefix <$> [ bytesEncode platform
                           , bytesEncode channelID
                           , T.pack (show addr) ]

instance VerifiableSignature DeletePushRequest where
    extractSignature (DeletePushRequest _ sig) = sig

    generateHash (DeletePushRequest addr _) = EU.hashText . T.concat $
        stripHexPrefix <$> [ T.pack (show addr) ]

instance VerifiableSignature PayPalRequest where
    extractSignature (PayPalRequest _ _ sig) = sig

    generateHash (PayPalRequest friend requestor _) = EU.hashText . T.concat $
        stripHexPrefix <$> [ T.pack (show friend)
                           , T.pack (show requestor) ]
