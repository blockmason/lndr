{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Lndr.CLI.Args
import           Lndr.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)


testUrl = "http://localhost:80"

testAddress1 = "198e13017d2333712bd942d8b028610b95c363da"
testAddress2 = "1ba7167373f13d28cc112f373bac8d5a07a47af9"

testNick1 = "test1"
testNick2 = "test2"

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Nicks"
            [ testCase "setting nicks and friends" nickTest
            ]
        , testGroup "Credits"
            [ testCase "lend money to friend" basicLendTest
            ]
        ]


nickTest :: Assertion
nickTest = do
    httpCode <- setNick testUrl (NickRequest testAddress1 testNick1 "")
    assertEqual "add friend success" 204 httpCode
    queriedNick <- getNick testUrl testAddress1
    assertEqual "nick is set and queryable" queriedNick testNick1
    httpCode <- setNick testUrl (NickRequest testAddress2 testNick1 "")
    assertBool "duplicate nick is rejected with user error" (httpCode /= 204)
    httpCode <- setNick testUrl (NickRequest testAddress1 testNick2 "")
    assertEqual "change nick success" 204 httpCode
    queriedNick <- getNick testUrl testAddress1
    assertEqual "nick is set and queryable" queriedNick testNick2
    -- add a friend to first account
    httpCode <- addFriend testUrl testAddress1 testAddress2
    assertEqual "add friend success" 204 httpCode
    -- verify that friend has been added
    nicks <- getFriends testUrl testAddress1
    assertEqual "friend properly added" ((\(NickInfo addr _) -> addr) <$> nicks) [testAddress2]

basicLendTest :: Assertion
basicLendTest = putStrLn "yet to be implemented"
