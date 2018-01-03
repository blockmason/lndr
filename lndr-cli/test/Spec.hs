{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Data.Either.Combinators (fromRight)
import qualified Data.Text.Lazy as LT
import           Lndr.CLI.Args
import           Lndr.NetworkStatistics
import           Lndr.Util (textToAddress, hashCreditRecord)
import           Lndr.Types
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Types
import qualified Network.Ethereum.Web3.Eth as Eth
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)

-- TODO get rid of this once version enpoint point works
ucacAddr = "0x7899b83071d9704af0b132859a04bb1698a3acaf"

testUrl = "http://localhost:80"
testPrivkey1 = "7231a774a538fce22a329729b03087de4cb4a1119494db1c10eae3bb491823e7"
testPrivkey2 = "f581608ccd4dcd78e341e464b86f268b77ee2673acc705023e64eeb5a4e31490"
testPrivkey3 = "b217205550c6011141e3580142ac43d7d41d217102f30e816eb36b70727e292e"
testPrivkey4 = "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea098"
testPrivkey5 = "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea099"
testPrivkey6 = "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea100"
testAddress1 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey1
testAddress2 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey2
testAddress3 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey3
testAddress4 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey4
testAddress5 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey5
testAddress6 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey6
testSearch = "test"
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
            , testCase "settlement" basicSettlementTest
            ]
        , testGroup "Admin"
            [ testCase "get and set gas price" basicGasTest
            ]
        , testGroup "Notifications"
            [ testCase "registerChannel" basicNotificationsTest
            ]
        ]


nickTest :: Assertion
nickTest = do
    -- check that nick is available
    nickTaken <- takenNick testUrl testNick1
    assertBool "after db reset all nicks are available" (not nickTaken)
    -- set nick for user1
    httpCode <- setNick testUrl (NickRequest testAddress3 testNick1 "")
    assertEqual "add friend success" 204 httpCode
    -- check that test nick is no longer available
    nickTaken <- takenNick testUrl testNick1
    assertBool "nicks already in db are not available" nickTaken
    -- check that nick for user1 properly set
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick1
    -- fail to set identical nick for user2
    httpCode <- setNick testUrl (NickRequest testAddress4 testNick1 "")
    assertBool "duplicate nick is rejected with user error" (httpCode /= 204)
    -- change user1 nick
    httpCode <- setNick testUrl (NickRequest testAddress3 testNick2 "")
    assertEqual "change nick success" 204 httpCode
    -- check that user1's nick was successfully changed
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick2

    -- set user2's nick
    httpCode <- setNick testUrl (NickRequest testAddress4 testNick1 "")
    assertEqual "previously used nickname is settable" 204 httpCode

    fuzzySearchResults <- searchNick testUrl testSearch
    assertEqual "search returns both results" 2 $ length fuzzySearchResults

    -- user1 adds user2 as a friend
    httpCode <- addFriend testUrl testAddress3 testAddress4
    assertEqual "add friend success" 204 httpCode
    -- verify that friend has been added
    friends <- getFriends testUrl testAddress3
    assertEqual "friend properly added" [NickInfo testAddress4 testNick1] friends

    -- user3 removes user4 from friends
    removeFriend testUrl testAddress3 testAddress4

    -- verify that friend has been removed
    friends <- getFriends testUrl testAddress3
    assertEqual "friend properly removed" [] friends


basicLendTest :: Assertion
basicLendTest = do
    let testCredit = CreditRecord testAddress1 testAddress2 100 "dinner" testAddress1 0 "" "" Nothing Nothing Nothing
        badTestCredit = CreditRecord testAddress1 testAddress1 100 "dinner" testAddress1 0 "" "" Nothing Nothing Nothing

        creditHash = hashCreditRecord ucacAddr (Nonce 0) testCredit

    -- user1 fails to submit pending credit to himself
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 badTestCredit False
    assertEqual "user1 cannot lend to himself" 400 httpCode

    -- user1 submits pending credit to user2
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit False
    assertEqual "lend success" 204 httpCode

    -- user1 checks pending transactions
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "one pending record found for user1" 1 (length creditRecords1)

    -- user2 checks pending transactions
    creditRecords2 <- checkPending testUrl testAddress2
    assertEqual "one pending record found for user2" 1 (length creditRecords2)

    -- user2 rejects pending transaction
    httpCode <- rejectCredit testUrl testPrivkey1 creditHash
    assertEqual "reject success" 204 httpCode

    -- user2 has 0 pending records post-rejection
    creditRecords2 <- checkPending testUrl testAddress2
    assertEqual "zero pending records found for user2" 0 (length creditRecords2)

    -- user1 attempts same credit again
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit False
    assertEqual "lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl ucacAddr testPrivkey2 (testCredit { submitter = testAddress2 }) False
    assertEqual "borrow success" 204 httpCode

    -- user1's checks that he has pending credits and one verified credit
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress1
    assertEqual "one verified record found for user1" 1 (length verifiedRecords1)

    balance <- getBalance testUrl testAddress1
    assertEqual "user1's total balance is 100" 100 balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress1 testAddress2
    assertEqual "user1's two-party balance is 100" 100 twoPartyBalance

    -- user1's counterparties list is [user2]
    counterparties <- getCounterparties testUrl testAddress1
    assertEqual "user1's counterparties properly calculated" [testAddress2] counterparties


basicSettlementTest :: Assertion
basicSettlementTest = do
    price <- queryEtheruemPrice
    assertBool "nonzero eth price retrieved from coinbase" (unPrice price > 0)

    let testCredit = CreditRecord testAddress5 testAddress6 100 "settlement" testAddress5 0 "" "" Nothing Nothing Nothing
        creditHash = hashCreditRecord ucacAddr (Nonce 0) testCredit

    -- user5 submits pending settlement credit to user6
    httpCode <- submitCredit testUrl ucacAddr testPrivkey5 testCredit True
    -- assertEqual "lend (settle) success" 204 httpCode
    print httpCode

    -- user6 accepts user5's pending settlement credit
    httpCode <- submitCredit testUrl ucacAddr testPrivkey6 (testCredit { submitter = testAddress6 }) True
    -- assertEqual "borrow (settle) success" 204 httpCode
    print httpCode

    -- user5 transfers eth to user6
    txHashE <- runWeb3 $ Eth.sendTransaction $ Call (Just testAddress5)
                                                    testAddress6
                                                    (Just 21000)
                                                    Nothing
                                                    (Just $ 10 ^ 18)
                                                    Nothing

    let txHash = fromRight (error "error sending eth") txHashE
    print txHash

    -- ensure that tx registers in blockchain w/ a 10 second pause
    threadDelay (10 ^ 7)

    -- user5 verifies that he has made the settlement credit
    httpCode <- verifySettlement testUrl creditHash txHash
    -- assertEqual "verification success" 204 httpCode
    print httpCode


basicGasTest :: Assertion
basicGasTest = do
    price <- getGasPrice testUrl

    -- double gas price
    httpCode <- setGasPrice testUrl testAddress1 (price * 2)
    assertEqual "add friend success" 204 httpCode

    -- check that gas price has been doubled
    newPrice <- getGasPrice testUrl
    assertEqual "gas price doubled" newPrice (price * 2)


basicNotificationsTest :: Assertion
basicNotificationsTest = do
    httpCode <- registerChannel testUrl testAddress1 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios")
    assertEqual "register channel success" 204 httpCode
