{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent             (threadDelay)
import           Control.Monad.Trans.Maybe
import           Data.Either.Combinators        (fromRight)
import           Data.Maybe                     (fromJust)
import qualified Data.Map                       as M
import qualified Data.Text.Lazy                 as LT
import           Lndr.CLI.Args
import           Lndr.Config
import           Lndr.EthereumInterface
import           Lndr.NetworkStatistics
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util                      ( parseIssueCreditInput,
                                                  textToAddress,
                                                  addHexPrefix)
import           Lndr.Web3
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Text.EmailAddress              as Email
import           System.Environment             (setEnv)
import           System.Directory

testUrl = "http://localhost:80"
testPrivkey0 = "7920ca01d3d1ac463dfd55b5ddfdcbb64ae31830f31be045ce2d51a305516a37"
testPrivkey1 = "bb63b692f9d8f21f0b978b596dc2b8611899f053d68aec6c1c20d1df4f5b6ee2"
testPrivkey2 = "2f615ea53711e0d91390e97cdd5ce97357e345e441aa95d255094164f44c8652"
testPrivkey3 = "7d52c3f6477e1507d54a826833169ad169a56e02ffc49a1801218a7d87ca50bd"
testPrivkey4 = "6aecd44fcb79d4b68f1ee2b2c706f8e9a0cd06b0de4729fe98cfed8886315256"
testPrivkey5 = "686e245584fdf696abd739c0e66ac6e01fc4c68babee20c7124566e118b2a634"
testPrivkey6 = "9fd4ab25e1699bb252f4d5c4510a135db34b3adca8baa03194ad5cd6faa13a1d"
testPrivkey7 = "e8445efa4e3349c3c74fd6689553f93b55aca723115fb777e1e6f4db2a0a82ca"
testPrivkey8 = "56901d80abc6953d1dc01de2f077b75260f49a3304f665b57ed13514a7e2a2bc"
testPrivkey9 = "edc63d0e14b29aaa26c7585e962f93abb59bd7d8b01b585e073dc03d052a000b"
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


loadUcacs = do
    (ConfigResponse ucacAddresses _ _ _ _) <- getConfig testUrl
    let Just ucacAddr = M.lookup "USD" ucacAddresses
        Just ucacAddrKRW = M.lookup "KRW" ucacAddresses
        Just ucacAddrJPY = M.lookup "JPY" ucacAddresses
    return (ucacAddr, ucacAddrKRW, ucacAddrJPY)


main :: IO ()
main = do
    setEnv web3ProviderEnvVariable "http://localhost:8545"
    defaultMain tests


tests :: [Test]
tests = [ testGroup "Nicks"
            [ testCase "setting nicks and friends" nickTest ]
        , testGroup "Credits"
            [ testCase "lend money to friend" basicLendTest
            , testCase "verify payment" verifySettlementTest
            , testCase "settlement" basicSettlementTest
            ]
        , testGroup "Admin"
            [ testCase "get current blocknumber" blocknumberTest
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
    assertEqual "friend properly added" [UserInfo testAddress4 (Just testNick1)] friends

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
    (ucacAddr, ucacAddrKRW, ucacAddrJPY) <- loadUcacs
    let testCredit' = CreditRecord testAddress1 testAddress2 100 "dinner" testAddress1 0 "" "" ucacAddr Nothing Nothing Nothing
        badTestCredit' = CreditRecord testAddress1 testAddress1 100 "dinner" testAddress1 0 "" "" ucacAddr Nothing Nothing Nothing

        creditHash = generateHash testCredit'
        testCredit = testCredit' { hash = creditHash }
        badTestCredit = badTestCredit' { hash = generateHash badTestCredit' }

    -- user1 fails to submit pending credit to himself
    httpCode <- submitCredit testUrl testPrivkey1 badTestCredit
    assertEqual "user1 cannot lend to himself" 400 httpCode

    -- user1 submits pending credit to user2
    httpCode <- submitCredit testUrl testPrivkey1 testCredit
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
    httpCode <- submitCredit testUrl testPrivkey1 testCredit
    assertEqual "lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl testPrivkey2 (testCredit { submitter = testAddress2 })
    assertEqual "borrow success" 204 httpCode

    -- user1's checks that he has pending credits and one verified credit
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress1
    assertEqual "one verified record found for user1" 1 (length verifiedRecords1)

    balance <- getBalance testUrl testAddress1 "USD"
    assertEqual "user1's total balance is 100" 100 balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress1 testAddress2 "USD"
    assertEqual "user1's two-party balance is 100" 100 twoPartyBalance

    -- user1's counterparties list is [user2]
    counterparties <- getCounterparties testUrl testAddress1
    assertEqual "user1's counterparties properly calculated" [testAddress2] counterparties

    -- user1 is friends with user2
    friends <- fmap addr <$> getFriends testUrl testAddress1
    assertEqual "user1's friends properly calculated" [testAddress2] friends

    -- user2 is friends with user1
    friends <- fmap addr <$> getFriends testUrl testAddress2
    assertEqual "user2's friends properly calculated" [testAddress1] friends

    -- user1 and user2 create credit on JPY ucac
    let jpyValue = 200000
        testCreditJPY' = CreditRecord testAddress2 testAddress1 jpyValue "sushi" testAddress2 0 "" "" ucacAddrJPY Nothing Nothing Nothing
        creditHashJPY = generateHash testCreditJPY'
        testCreditJPY = testCreditJPY' { hash = creditHashJPY }

    -- user2 submits pending credit to user1
    httpCode <- submitCredit testUrl testPrivkey2 testCreditJPY
    assertEqual "lend success" 204 httpCode

    -- user1 accepts user2's pending credit
    httpCode <- submitCredit testUrl testPrivkey1 (testCreditJPY { submitter = testAddress1 })
    assertEqual "borrow success" 204 httpCode

    -- user2 has a correct total JPY balance
    balance <- getBalance testUrl testAddress2 "JPY"
    assertEqual "user2's total jpy balance is correct" jpyValue balance

    balance <- getTwoPartyBalance testUrl testAddress1 testAddress2 "JPY"
    assertEqual "user2 & user1's two-party total balance is correct" (-jpyValue) balance


basicSettlementTest :: Assertion
basicSettlementTest = do
    (ucacAddr, ucacAddrKRW, ucacAddrJPY) <- loadUcacs
    pricesM <- runMaybeT queryEtheruemPrices
    case pricesM of
        Just prices -> assertBool "nonzero eth price retrieved from coinbase" (usd prices > 0)
        Nothing -> return ()

    let testCredit' = CreditRecord testAddress5 testAddress6 100 "settlement" testAddress5 0 "" "" ucacAddr Nothing (Just "ETH") Nothing
        creditHash = generateHash testCredit'
        testCredit = testCredit' { hash = creditHash }

    -- user5 submits pending settlement credit to user6
    httpCode <- submitCredit testUrl testPrivkey5 testCredit
    assertEqual "lend (settle) success" 204 httpCode

    -- check that pending settlement is registered in test
    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "pre-confirmation: get pending settlements success" 1 (length pendingSettlements)
    assertEqual "pre-confirmation: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    -- user6 accepts user5's pending settlement credit
    httpCode <- submitCredit testUrl testPrivkey6 (testCredit { submitter = testAddress6 })
    assertEqual "borrow (settle) success" 204 httpCode

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "post-confirmation: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-confirmation: get bilateral pending settlements success" 1 (length bilateralPendingSettlements)

    let settleAmount = fmap Quantity . settlementAmount . creditRecord $ head bilateralPendingSettlements

    -- user5 transfers eth to user6
    txHashE <- runLndrWeb3 $ Eth.sendTransaction $ Call (Just testAddress5)
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

    -- ensure that tx registers in blockchain w/ a 7 second pause and
    -- heartbeat has time to verify its validity
    threadDelay (7 * 10 ^ 6)

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getSettlements testUrl testAddress5
    assertEqual "post-verification: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-verification: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    gottenTxHash <- getTxHash testUrl creditHash
    assertEqual "successful txHash retrieval" txHash (addHexPrefix gottenTxHash)

    (dbCredits, blockchainCredits, _) <- getUnsubmitted testUrl
    assertBool "equal, non-zero number of transactions in db and blockchain" (dbCredits == blockchainCredits && dbCredits == 3)


verifySettlementTest :: Assertion
verifySettlementTest = do

    syncingE <- runLndrWeb3 Eth.syncing
    assertEqual "confirm that blockchain is synced" syncingE (Right NotSyncing)

    (ucacAddr, _, _) <- loadUcacs
    let settleAmountInWei = 10 ^ 18
    -- testAddress1 is the person revieving eth, thus the credit must record
    -- this address as the debtor.
    txHashE <- runLndrWeb3 $ Eth.sendTransaction $ Call (Just testAddress4)
                                                        testAddress1
                                                        (Just 21000)
                                                        Nothing
                                                        (Just settleAmountInWei)
                                                        Nothing
    let txHash = fromRight (error "error sending eth") txHashE

    threadDelay (5 * 10 ^ 6)

    verified <- verifySettlementPayment (BilateralCreditRecord ( CreditRecord testAddress4
                                                                              testAddress1
                                                                              10
                                                                              ""
                                                                              testAddress4
                                                                              0
                                                                              ""
                                                                              ""
                                                                              ucacAddr
                                                                              (Just $ unQuantity settleAmountInWei)
                                                                              (Just "ETH")
                                                                              (Just 0)
                                                               ) "" "" (Just txHash))
    -- txHash testAddress4 testAddress1 (10 ^ 18)
    assertBool "payment properly verified" verified


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
nickSignTest = assertEqual "expected nick request signature" nickSignature (Right "6c965e1e501c18eedaf8af07dcdee651e0569efd017211be7faf10454c11bcf9611c8e9feaca7ac8ac400cce441db84ecbeb6f817fd8f40e321b33c3a0f4b21d1b")
    where
        unsignedNickRequest = NickRequest testAddress1 "testNick" ""
        nickSignature = generateSignature unsignedNickRequest testPrivkey1
