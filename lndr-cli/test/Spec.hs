{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent             (threadDelay)
import           Control.Monad.Trans.Maybe
import           Data.Either.Combinators        (fromRight)
import           Data.Maybe                     (fromJust)
import qualified Data.Text.Lazy                 as LT
import           Lndr.CLI.Args
import           Lndr.EthereumInterface
import           Lndr.NetworkStatistics
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util                      (hashCreditRecord,
                                                 parseIssueCreditInput,
                                                 textToAddress,
                                                 addHexPrefix)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Text.EmailAddress              as Email

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
testEmailText = "tim@blockmason.io"
testEmail = fromJust $ Email.emailAddressFromText testEmailText

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Nicks"
            [ testCase "setting nicks and friends" nickTest ]
        , testGroup "Credits"
            [ testCase "lend money to friend" basicLendTest
            , testCase "verify payment" verifySettlementTest
            , testCase "settlement" basicSettlementTest
            ]
        , testGroup "Admin"
            [ testCase "get and set gas price" gasTest
            , testCase "get current blocknumber" blocknumberTest
            ]
        , testGroup "Notifications"
            [ testCase "registerChannel" basicNotificationsTest
            ]
        , testGroup "Authentication"
            [ testCase "nick signing" nickSignTest
            ]
        , testGroup "Utils"
            [ testCase "parseIssueCreditInput" parseCreditInputTest
            ]
        ]


nickTest :: Assertion
nickTest = do
    -- check that nick is available
    nickTaken <- takenNick testUrl testNick1
    assertBool "after db reset all nicks are available" (not nickTaken)
    -- set nick for user1
    httpCode <- setNick testUrl testPrivkey3 (NickRequest testAddress3 testNick1 "")
    assertEqual "set nick success" 204 httpCode
    -- check that test nick is no longer available
    nickTaken <- takenNick testUrl testNick1
    assertBool "nicks already in db are not available" nickTaken
    -- check that nick for user1 properly set
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick1
    -- fail to set identical nick for user2
    httpCode <- setNick testUrl testPrivkey4 (NickRequest testAddress4 testNick1 "")
    assertBool "duplicate nick is rejected with user error" (httpCode /= 204)
    -- change user1 nick
    httpCode <- setNick testUrl testPrivkey3 (NickRequest testAddress3 testNick2 "")
    assertEqual "change nick success" 204 httpCode
    -- check that user1's nick was successfully changed
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick2

    -- set user2's nick
    httpCode <- setNick testUrl testPrivkey4 (NickRequest testAddress4 testNick1 "")
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

    -- EMAIL TESTS

    -- check that email isn't taken
    emailTaken <- takenEmail testUrl testEmailText
    assertBool "after db reset all emails are available" (not emailTaken)

    -- set email for user1
    httpCode <- setEmail testUrl testPrivkey3 (EmailRequest testAddress3 testEmail "")
    assertEqual "set email success" 204 httpCode

    -- check that testEmail is no longer available
    emailTaken <- takenEmail testUrl testEmailText
    assertBool "emails already in db are not available" emailTaken

    -- check that email for user3 properly set
    queriedEmail <- getEmail testUrl testAddress3
    assertEqual "email is set and queryable" queriedEmail testEmail


basicLendTest :: Assertion
basicLendTest = do
    let testCredit = CreditRecord testAddress1 testAddress2 100 "dinner" testAddress1 0 "" "" Nothing Nothing Nothing
        badTestCredit = CreditRecord testAddress1 testAddress1 100 "dinner" testAddress1 0 "" "" Nothing Nothing Nothing

        creditHash = hashCreditRecord ucacAddr (Nonce 0) testCredit

    -- user1 fails to submit pending credit to himself
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 badTestCredit
    assertEqual "user1 cannot lend to himself" 400 httpCode

    -- user1 submits pending credit to user2
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit
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
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit
    assertEqual "lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl ucacAddr testPrivkey2 (testCredit { submitter = testAddress2 })
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
    pricesM <- runMaybeT queryEtheruemPrices
    case pricesM of
        Just prices -> assertBool "nonzero eth price retrieved from coinbase" (usd prices > 0)
        Nothing -> return ()

    let testCredit = CreditRecord testAddress5 testAddress6 100 "settlement" testAddress5 0 "" "" Nothing (Just "ETH") Nothing
        creditHash = hashCreditRecord ucacAddr (Nonce 0) testCredit

    -- user5 submits pending settlement credit to user6
    httpCode <- submitCredit testUrl ucacAddr testPrivkey5 testCredit
    assertEqual "lend (settle) success" 204 httpCode

    -- check that pending settlement is registered in test
    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "pre-confirmation: get pending settlements success" 1 (length pendingSettlements)
    assertEqual "pre-confirmation: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    -- user6 accepts user5's pending settlement credit
    httpCode <- submitCredit testUrl ucacAddr testPrivkey6 (testCredit { submitter = testAddress6 })
    assertEqual "borrow (settle) success" 204 httpCode

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "post-confirmation: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-confirmation: get bilateral pending settlements success" 1 (length bilateralPendingSettlements)

    let settleAmount = fmap Quantity . settlementAmount . settlementCreditRecord $ head bilateralPendingSettlements

    -- user5 transfers eth to user6
    txHashE <- runWeb3 $ Eth.sendTransaction $ Call (Just testAddress5)
                                                    testAddress6
                                                    (Just 21000)
                                                    Nothing
                                                    settleAmount
                                                    Nothing

    let txHash = fromRight (error "error sending eth") txHashE

    httpCode <- getTxHashFail testUrl creditHash
    assertEqual "404 upon hash not found error" 404 httpCode

    -- user5 verifies that he has made the settlement credit
    httpCode <- verifySettlement testUrl creditHash txHash testPrivkey5
    assertEqual "verification success" 204 httpCode

    -- ensure that tx registers in blockchain w/ a 10 second pause and
    -- heartbeat has time to verify its validity
    threadDelay (8 * 10 ^ 6)

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "post-verification: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-verification: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    gottenTxHash <- getTxHash testUrl creditHash
    assertEqual "successful txHash retrieval" txHash (addHexPrefix gottenTxHash)


verifySettlementTest :: Assertion
verifySettlementTest = do
    -- testAddress1 is the person revieving eth, thus the credit must record
    -- this address as the debtor.
    txHashE <- runWeb3 $ Eth.sendTransaction $ Call (Just testAddress4)
                                                    testAddress1
                                                    (Just 21000)
                                                    Nothing
                                                    (Just $ 10 ^ 18)
                                                    Nothing
    let txHash = fromRight (error "error sending eth") txHashE

    threadDelay (5 * 10 ^ 6)

    verified <- verifySettlementPayment txHash testAddress4 testAddress1 (10 ^ 18)
    assertBool "payment properly verified" verified


gasTest :: Assertion
gasTest = do
    price <- getGasPrice testUrl

    -- double gas price
    httpCode <- setGasPrice testUrl testAddress1 (price * 2)
    assertEqual "add friend success" 204 httpCode

    -- check that gas price has been doubled
    newPrice <- getGasPrice testUrl
    assertEqual "gas price doubled" newPrice (price * 2)


blocknumberTest :: Assertion
blocknumberTest = do
    blockNumberM <- runMaybeT currentBlockNumber

    case blockNumberM of
        Just blockNumber -> assertBool "block number within expected bounds" (blockNumber < 4000)
        Nothing -> return ()


basicNotificationsTest :: Assertion
basicNotificationsTest = do
    httpCode <- registerChannel testUrl testPrivkey1 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios" testAddress1 "")
    assertEqual "register channel success" 204 httpCode


parseCreditInputTest :: Assertion
parseCreditInputTest = do
        assertEqual "expected credit log" creditLog (IssueCreditLog "869a8f2c3d22be392618ed06c8f548d1d5b5aed6" "754952bfa2097104a07f4f347e513a1da576ac7a" "3a1ea286e419130d894c9fa0cf49898bc81f9a5a" 2617 0 "fedex            ")
        assertBool "good creditor sig" goodCreditorSig
        assertBool "good debtor sig" goodDebtorSig
    where (creditLog, _, goodCreditorSig, _, goodDebtorSig) = parseIssueCreditInput (Nonce 0) "0x0a5b410e000000000000000000000000869a8f2c3d22be392618ed06c8f548d1d5b5aed6000000000000000000000000754952bfa2097104a07f4f347e513a1da576ac7a0000000000000000000000003a1ea286e419130d894c9fa0cf49898bc81f9a5a0000000000000000000000000000000000000000000000000000000000000a394988c5614ea5a5807387af10ab3520ed0bb9e8edfcef07924b3630b119dccab12981d3efa478582c5c69a4c9ef5ccb3e019e52cd2b210cc0bc17133799d1f739000000000000000000000000000000000000000000000000000000000000001cd58818da99967a502e7abc5cc74b3569063c7670924d22e25c329b560298cc5566e07b7c87b7a8685f954410480fe2f5cd08dbcb84552dcc3b4475e8469f1b12000000000000000000000000000000000000000000000000000000000000001c6665646578202020202020202020202020202020202020202020202020202020"


nickSignTest :: Assertion
nickSignTest = assertEqual "expected nick request signature" nickSignature (Right "56324c5c3b52210099f12e569e73ee1853524b958cebc60a33ad1cfd32a84cf56caddd36ead0c2c34ebb0188e2a9fbf4591f3c1d34d3ba8bfe5ef2dae513a38a1c")
    where
        unsignedNickRequest = NickRequest testAddress1 "testNick" ""
        nickSignature = generateSignature unsignedNickRequest testPrivkey1
