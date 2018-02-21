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

instance FromRow NickInfo

instance FromField DevicePlatform where
    fromField f dat = toDevicePlatform <$> fromField f dat
        where toDevicePlatform :: Text -> DevicePlatform
              toDevicePlatform "ios" = Ios
              toDevicePlatform "android" = Android

instance FromRow CreditRecord where
    fromRow = do
       baseCredit <- CreditRecord <$> field <*> field <*> ((floor :: Rational -> Integer) <$> field)
                                  <*> field <*> field
                                  <*> ((floor :: Rational -> Integer) <$> field) <*> field <*> field
                                  <*> field
       remaining <- numFieldsRemaining
       if remaining == 0 then return $ baseCredit Nothing Nothing Nothing
                         else baseCredit <$> (fmap (floor :: Rational -> Integer) <$> field) <*> field
                                         <*> (fmap (floor :: Rational -> Integer) <$> field)

instance FromRow BilateralCreditRecord where
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
        return $ BilateralCreditRecord
            -- TODO make this function more flexible to accept settlment
            -- credits as well
            (CreditRecord creditor debtor amount memo submitter nonce hash signature ucac Nothing Nothing Nothing)
            creditorSignature
            debtorSignature

instance ToRow BilateralCreditRecord where
    toRow (BilateralCreditRecord creditRecord creditorSignature debtorSignature) =
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


instance FromRow SettlementCreditRecord where
  fromRow = do
    creditRecord <- CreditRecord <$> field <*> field <*> ((floor :: Rational -> Integer) <$> field)
                                 <*> field <*> field
                                 <*> ((floor :: Rational -> Integer) <$> field) <*> field <*> field
                                 <*> field
                                 <*> (fmap (floor :: Rational -> Integer) <$> field) <*> field
                                 <*> (fmap (floor :: Rational -> Integer) <$> field)
    SettlementCreditRecord creditRecord <$> field

instance FromRow IssueCreditLog where
  fromRow =
    IssueCreditLog <$> field <*> field <*> field <*> ((floor :: Rational -> Integer) <$> field)
                   <*> ((floor :: Rational -> Integer) <$> field) <*> field
