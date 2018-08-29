{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Types where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3

-- DB Typeclass instances

instance ToField Address where
    toField = Escape . addrToBS

instance FromField Address where
    fromField f dat = textToAddress <$> fromField f dat

instance FromRow UserInfo

instance FromRow PayPalRequestPair where
    fromRow = do
        friendAddr <- field
        requestorAddr <- field
        friendNick <- field
        requestorNick <- field
        let friend = UserInfo friendAddr friendNick
            requestor = UserInfo requestorAddr requestorNick

        return $ PayPalRequestPair friend requestor
        
instance FromField DevicePlatform where
    fromField f dat = toDevicePlatform <$> fromField f dat
        where toDevicePlatform :: Text -> DevicePlatform
              toDevicePlatform "ios" = Ios
              toDevicePlatform "android" = Android

instance FromRow CreditRecord where
    fromRow = do
       baseCredit <- CreditRecord <$> field <*> field
                                  <*> ((floor :: Rational -> Integer) <$> field)
                                  <*> field <*> field
                                  <*> ((floor :: Rational -> Integer) <$> field)
                                  <*> field <*> field <*> field <*> field
       remaining <- numFieldsRemaining
       if remaining == 0
            then return $ baseCredit Nothing Nothing
            else baseCredit <$> (fmap (floor :: Rational -> Integer) <$> field)
                            <*> (fmap (floor :: Rational -> Integer) <$> field)

instance FromRow BilateralCreditRecord where
    -- 'fromRow' from 'CreditRecord' cannot be reused here due to the necessity
    -- of calculating signature from creditorSig / debtorSig
    fromRow = do
        creditor <- field
        debtor <- field
        amount <- (floor :: Rational -> Integer) <$> field
        memo <- field
        submitter <- field
        nonce <- (floor :: Rational -> Integer) <$> field
        hash <- field
        ucac <- field
        creditorSignature <- field
        debtorSignature <- field
        let signature = if submitter == creditor
                            then creditorSignature
                            else debtorSignature
        remaining <- numFieldsRemaining
        (settlementCurrencyM, settlementAmountM, settlementBlockNumberM, txHashM) <-
            if remaining == 0
                then pure (Nothing, Nothing, Nothing, Nothing)
                else (,,,) <$> (fmap (floor :: Rational -> Integer) <$> field)
                           <*> field
                           <*> (fmap (floor :: Rational -> Integer) <$> field)
                           <*> field
        return $ BilateralCreditRecord
            (CreditRecord creditor debtor amount memo submitter nonce hash signature ucac
                          settlementAmountM settlementCurrencyM settlementBlockNumberM)
            creditorSignature debtorSignature txHashM

instance ToRow BilateralCreditRecord where
    toRow (BilateralCreditRecord creditRecord creditorSignature debtorSignature _) =
        [ toField $ creditor creditRecord
        , toField $ debtor creditRecord
        , toField $ amount creditRecord
        , toField $ memo creditRecord
        , toField $ nonce creditRecord
        , toField $ hash creditRecord
        , toField creditorSignature
        , toField debtorSignature
        , toField $ ucac creditRecord
        , toField $ submitter creditRecord
        ]

instance ToRow Address where
    toRow address = [ toField address ]

instance FromRow IssueCreditLog where
  fromRow =
    IssueCreditLog <$> field <*> field <*> field
                   <*> ((floor :: Rational -> Integer) <$> field)
                   <*> ((floor :: Rational -> Integer) <$> field) <*> field

instance ToRow IssueCreditLog where
    toRow cl@(IssueCreditLog ucac creditor debtor amount nonce memo) =
        [ toField creditor
        , toField debtor
        , toField amount
        , toField memo
        , toField nonce
        , toField $ hashCreditLog cl
        , toField ("" :: Text)
        , toField ("" :: Text)
        , toField ucac
        , toField creditor
        ]

instance ToRow VerificationStatusEntry where
    toRow entry@(VerificationStatusEntry user applicantId status) = 
        [ toField user
        , toField applicantId
        , toField status
        ]

instance FromRow VerificationStatusEntry where
    fromRow =
        VerificationStatusEntry <$> field <*> field <*> field
