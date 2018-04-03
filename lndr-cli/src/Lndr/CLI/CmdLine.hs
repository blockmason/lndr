{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lndr.CLI.CmdLine (
      LndrCmd(..)
    , programModes
    ) where

import           Data.Text                       (Text)
import           System.Console.CmdArgs          hiding (def)
import           System.Console.CmdArgs.Explicit (HelpFormat (..), helpText,
                                                  modeEmpty)

data LndrCmd = Transactions
             | Pending
             | RejectPending
             | Lend { friend :: Text
                    , amount :: Integer
                    , memo   :: Text
                    }
             | Borrow { friend :: Text
                      , amount :: Integer
                      , memo   :: Text
                      }
             | Nick { nick :: Text }
             | SearchNick { nick :: Text }
             | GetNonce { friend :: Text }
             | AddFriend { friend :: Text }
             | RemoveFriend { friend :: Text }
             | SetPhoto { photoPath :: String }
             | Info
             | Unsubmitted
             | PendingSettlements
             | LndrConfig
             | ScanBlockchain
             deriving (Show, Data, Typeable)


programModes = modes [ Transactions
                        &= help "List all transactions involving default user in default Lndr UCAC"
                     , Pending
                        &= help "List all pending transactions"
                     , RejectPending
                        &= help "Start interactive credit rejection proecss"
                     , Lend { friend = "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf" &= typ "<counterparty address>"
                            , amount = 123 &= typ "<currency units>"
                            , memo = "default" &= typ "<memo text>"
                            }
                        &= help "Submit a unilateral transaction as a creditor"
                     , Borrow { friend = "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf" &= typ "<counterparty address>"
                              , amount = 123 &= typ "<currency units>"
                              , memo = "default" &= typ "<memo text>"
                              }
                        &= help "Submit a unilateral transaction as a debtor"
                     , Nick "aupiff"
                        &= help "Set a nickname for default user"
                     , SearchNick "aupiff"
                        &= help "Find address for a corresponding nickname"
                     , GetNonce { friend = "0x198e13017d2333712bd942d8b028610b95c363da"
                                    &= typ "<friend>"
                                }
                        &= help "Display nonce between default user and the indicated counterparty addess"
                     , AddFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                        &= help "Display nonce between default user and the indicated counterparty addess"
                     , RemoveFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                        &= help "Remove a friend with the indicated address\
                               \ from the default user's friend list"
                     , SetPhoto { photoPath = "image.jpeg" &= typ "<image file>" }
                        &= help "Use a particular image file as the default\
                               \ user's profile photo"
                     , Unsubmitted
                        &= help "Prints txs that are in lndr db but not yet on the blockchain"
                     , Info
                        &= help "Prints config, nick, and friends"
                     , PendingSettlements
                        &= help "List all pending settlements"
                     , LndrConfig
                        &= help "Prints config endpoint response"
                     , ScanBlockchain
                        &= help "Scan blockchain for all CP transactions"
                     ] &= help "Lend and borrow money.\nServer URL, default user,\
                              \ and default ucac must be indicated in configuration file."
                       &= program "lndr"
                       &= summary "lndr v0.1"
