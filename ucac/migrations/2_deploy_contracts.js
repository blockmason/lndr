const fs = require('fs');

const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const Lndr = artifacts.require('./Lndr.sol');
const LndrJPY = artifacts.require('./LndrJPY.sol');
const LndrKRW = artifacts.require('./LndrKRW.sol');

module.exports = async function (deployer, network, accounts) {
    if (network == "gethtest" || network == "testrpc") {
        const usd = web3.fromAscii("USD");
        const jpy = web3.fromAscii("JPY");
        const krw = web3.fromAscii("KRW");
        const mintAmount = web3.toBigNumber(web3.toWei(20000))
        var cpTokenContract;
        var creditProtocolContract;
        var lndrUsdContract;
        var lndrJpyContract;
        var lndrKrwContract;

        await deployer.deploy(CPToken,{from: web3.eth.accounts[0]})
        cpTokenContract = await CPToken.deployed();;
        await deployer.deploy( CreditProtocol
                             , cpTokenContract.address
                             , web3.toBigNumber(2 * 10 ** 9)
                             , web3.toBigNumber(1)
                             , {from: web3.eth.accounts[0]});
        creditProtocolContract = await CreditProtocol.deployed();
        await deployer.deploy(Lndr, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrJPY, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrKRW, {from: web3.eth.accounts[0]});
        lndrUsdContract = await Lndr.deployed();
        lndrJpyContract = await LndrJPY.deployed();
        lndrKrwContract = await LndrKRW.deployed();
        // mint tokens for accounts[0] to stake fid
        await cpTokenContract.mint( web3.eth.accounts[0]
                                  , web3.toWei(2000)
                                  , {from: web3.eth.accounts[0]});
        await cpTokenContract.finishMinting({from: web3.eth.accounts[0]});
        await cpTokenContract.endSale({from: web3.eth.accounts[0]});
        await cpTokenContract.approve(creditProtocolContract.address, web3.toWei(300), {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrUsdContract.address
                                                       , usd
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrJpyContract.address
                                                       , jpy
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrKrwContract.address
                                                       , krw
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
    }
};
