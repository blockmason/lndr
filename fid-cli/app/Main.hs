{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import           System.Console.CmdArgs

import           Web3Interface

data FiDCmd = Transactions
            | Pending
            | Lend { me :: Text
                   , friend :: Text
                   , amount :: Integer
                   }
            | Borrow { me :: Text
                     , friend :: Text
                     , amount :: Integer
                     }
            deriving (Show, Data, Typeable)

-- TODO put this in ReaderT to handle config vars loaded at runtime
-- (config var to implement: web address, userid)
runMode :: FiDCmd -> IO ()
runMode Transactions = do resp <- HTTP.httpJSON "http://localhost:80/transactions"
                          -- TODO prettyprint
                          print $ (HTTP.getResponseBody resp :: [IssueCreditLog])
runMode Pending = do resp <- HTTP.httpJSON "http://localhost:80/pending"
                          -- TODO prettyprint
                     print $ (HTTP.getResponseBody resp :: [(Text, CreditRecord Signed)])
runMode (Lend user friend amount) = submitCredit $ CreditRecord user friend amount user
runMode (Borrow user friend amount) = submitCredit $ CreditRecord friend user amount user


submitCredit :: CreditRecord Unsigned -> IO ()
submitCredit unsignedCredit = do
    initReq <- HTTP.parseRequest "http://localhost:80/submit"
    let req = HTTP.setRequestBodyJSON unsignedCredit $
                HTTP.setRequestMethod "POST" initReq
    resp <- HTTP.httpJSON req
    print $ (HTTP.getResponseBody resp :: SubmissionResponse)


programModes =  (modes [ Transactions
                       , Pending
                       , Lend "0x198e13017d2333712bd942d8b028610b95c363da"
                              "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                              123
                       , Borrow "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                                "0x198e13017d2333712bd942d8b028610b95c363da"
                                123
                       ])


main :: IO ()
main = do mode <- cmdArgsRun $ cmdArgsMode programModes
          runMode $ mode
