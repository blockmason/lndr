const fs = require('fs');

const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const Lndr = artifacts.require('./Lndr.sol');

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
