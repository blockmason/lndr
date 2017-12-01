const fs = require('fs');

const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const Lndr = artifacts.require('./Lndr.sol');

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


function stripHex(addr) {
    return addr.substr(2, addr.length);
}

function creditHash(ucacAddr, p1, p2, amount, nonce) {
    return [ucacAddr, p1, p2, amount, nonce].map(stripHex).join("")
}

function bignumToHexString(num) {
    const a = num.toString(16);
    return "0x" + '0'.repeat(64 - a.length) + a;
}

async function makeTransaction(cp, ucacAddr, creditor, debtor, _amount) {
    let nonce = creditor < debtor ? await cp.nonces(creditor, debtor) : await cp.nonces(debtor, creditor);
    nonce = bignumToHexString(nonce);
    let amount = bignumToHexString(_amount);
    let content = creditHash(ucacAddr, creditor, debtor, amount, nonce);
    let sig1 = sign(creditor, content);
    let sig2 = sign(debtor, content);
    let txReciept = await cp.issueCredit( ucacAddr, creditor, debtor, amount
                                   , [ sig1.r, sig1.s, sig1.v ]
                                   , [ sig2.r, sig2.s, sig2.v ]
                                   , testMemo, {from: creditor});
    return txReciept;
}

module.exports = function(deployer, network, accounts) {
    if (network == "gethtest") {
        const usd = web3.fromAscii("USD");
        const mintAmount = web3.toBigNumber(web3.toWei(20000))
        var cpTokenContract;
        var creditProtocolContract;
        var fidContract;

        deployer.deploy(CPToken,{from: web3.eth.accounts[0]}).then(() => {
            return CPToken.deployed();
        }).then(_cpTokenContract => {
            cpTokenContract = _cpTokenContract;
            return deployer.deploy( CreditProtocol
                                  , cpTokenContract.address
                                  , web3.toBigNumber(2 * 10 ** 9)
                                  , web3.toBigNumber(1)
                                  , {from: web3.eth.accounts[0]});
        }).then(() => {
            return CreditProtocol.deployed();
        }).then(_creditProtocolContract => {
            creditProtocolContract = _creditProtocolContract;
            return deployer.deploy(Lndr, {from: web3.eth.accounts[0]});
        }).then(() => {
            return Lndr.deployed();
        }).then(_fidContract => {
            fidContract = _fidContract;
            // mint tokens for accounts[0] to stake fid
            return cpTokenContract.mint( web3.eth.accounts[0]
                                       , web3.toWei(2000)
                                       , {from: web3.eth.accounts[0]});
        }).then(() => {
            return cpTokenContract.finishMinting({from: web3.eth.accounts[0]});
        }).then(() => {
            return cpTokenContract.endSale({from: web3.eth.accounts[0]});
        }).then(() => {
            return cpTokenContract.approve(creditProtocolContract.address, web3.toWei(100), {from: web3.eth.accounts[0]});
        }).then(() => {
            return creditProtocolContract.createAndStakeUcac( fidContract.address
                                                            , usd
                                                            , web3.toWei(100)
                                                            , {from: web3.eth.accounts[0]});
        }).catch(function(e) {
            console.log(e);
        });
    }
};
