{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.Identity where

import           Control.Monad
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Lndr.Db.Types
import           Lndr.Types
import           Network.Ethereum.Web3
import           Text.EmailAddress
import qualified Text.EmailAddress as Email

addVerificationStatus :: VerificationStatusEntry -> Connection -> IO Int
addVerificationStatus statusValues@(VerificationStatusEntry user id status) conn = fromIntegral <$>
    execute conn "INSERT INTO identity_verification (user, applicant_id, status) VALUES (?,?,?) ON CONFLICT (user) DO UPDATE SET status = EXCLUDED.status" (user, id, status)


removeVerificationStatus :: Address -> Connection -> IO Int
removeVerificationStatus addr conn = fromIntegral <$>
    execute conn "DELETE FROM identity_verification WHERE address = ?" (Only addr)


lookupVerificationStatus :: Address -> Connection -> IO (Maybe VerificationStatusEntry)
lookupVerificationStatus addr conn = listToMaybe <$>
    (query conn "SELECT address, applicant_id, status FROM identity_verification WHERE address = ?" (Only addr) :: IO [VerificationStatusEntry])
