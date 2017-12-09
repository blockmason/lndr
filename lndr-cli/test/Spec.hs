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

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testCase "nick" nickTest
        , testCase "add friend" addFriendTest
        ]


nickTest :: Assertion
nickTest = do
    setNick testUrl (NickRequest testAddress testNick "")
    queriedNick <- getNick testUrl testAddress
    assertEqual "nick is set and queryable" queriedNick testNick
    where
        testNick = "test"

addFriendTest :: Assertion
addFriendTest = putStrLn "to be implemented"
