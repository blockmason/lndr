{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Lndr.CLI.Args
import           Lndr.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)


testUrl = "http://localhost:80"

testAddress = "198e13017d2333712bd942d8b028610b95c363da"

testNick = "test"

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Signing"
            [ testCase "setting nick" setNickTest
            , testCase "duplicate nick rejected" duplicateNickTest
            ]
        , testGroup "Friends"
            [ testCase "add friend" addFriendTest ]
        ]


setNickTest :: Assertion
setNickTest = do
    setNick testUrl (NickRequest testAddress testNick "")
    queriedNick <- getNick testUrl testAddress
    assertEqual "nick is set and queryable" queriedNick testNick


duplicateNickTest :: Assertion
duplicateNickTest = do
    httpCode <- setNick testUrl (NickRequest testAddress testNick "")
    assertEqual "duplicate nick is rejected with user error" 400 httpCode


addFriendTest :: Assertion
addFriendTest = putStrLn "to be implemented"
