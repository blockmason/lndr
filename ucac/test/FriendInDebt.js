const should = require('chai')
          .use(require('chai-as-promised'))
          .use(require('chai-bignumber')(web3.BigNumber))
          .should();

const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const FriendInDebt = artifacts.require('./FriendInDebt.sol');

const usd = web3.fromAscii("USD");
const ucacId1 = web3.sha3("hi");
const ucacId2 = web3.sha3("yo");
const creationStake = web3.toBigNumber(web3.toWei(3500));
const mintAmount = web3.toBigNumber(web3.toWei(20000))

function sign(signer, content) {
    let contentHash = web3.sha3(content, {encoding: 'hex'});
    let sig = web3.eth.sign(signer, contentHash, {encoding: 'hex'});
    sig = sig.substr(2, sig.length);

    let res = {};
    res.r = "0x" + sig.substr(0, 64);
    res.s = "0x" + sig.substr(64, 64);
    res.v = web3.toDecimal("0x" + sig.substr(128, 2));

    if (res.v < 27) res.v += 27;

    return res;
}

function bignumToHexString(num) {
    const a = num.toString(16);
    return "0x" + '0'.repeat(64 - a.length) + a;
}

async function makeTransaction(cp, ucacId, creditor, debtor, _amount) {
    let nonce = creditor < debtor ? await cp.nonces(creditor, debtor) : await cp.nonces(debtor, creditor);
    nonce = bignumToHexString(nonce);
    let amount = bignumToHexString(_amount);
    let content1 = ucacId + creditor.substr(2, creditor.length) + debtor.substr(2, debtor.length)
                          + amount.substr(2, amount.length) + nonce.substr(2, nonce.length);
    let sig1 = sign(creditor, content1);
    let content2 = ucacId + creditor.substr(2, creditor.length) + debtor.substr(2, debtor.length)
                          + amount.substr(2, amount.length) + nonce.substr(2, nonce.length);
    let sig2 = sign(debtor, content2);
    let txReciept = await cp.issueCredit( ucacId, creditor, debtor, amount
                                   , sig1.r, sig1.s, sig1.v
                                   , sig2.r, sig2.s, sig2.v, {from: creditor});
    return txReciept;
}

contract('CreditProtocolTest', function([admin, p1, p2, ucacAddr]) {

    before(async function() {
    });

    beforeEach(async function() {
        this.cpToken = await CPToken.new({from: admin});
        this.creditProtocol =
            await CreditProtocol.new( this.cpToken.address
                                    , web3.toBigNumber(2 * 10 ** 9)
                                    , web3.toBigNumber(1)
                                    , {from: admin});
    });

    describe("Friends in Debt", () => {

        it("`stakeTokens` stakes appropriate number of tokens for initialized ucacs", async function() {
        });

    });

});
