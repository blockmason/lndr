const fs = require('fs');

const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const Lndr = artifacts.require('./Lndr.sol');
const LndrJPY = artifacts.require('./LndrJPY.sol');
const LndrKRW = artifacts.require('./LndrKRW.sol');
const LndrAUD = artifacts.require('./LndrAUD.sol');
const LndrCAD = artifacts.require('./LndrCAD.sol');
const LndrCHF = artifacts.require('./LndrCHF.sol');
const LndrCNY = artifacts.require('./LndrCNY.sol');
const LndrDKK = artifacts.require('./LndrDKK.sol');
const LndrEUR = artifacts.require('./LndrEUR.sol');
const LndrGBP = artifacts.require('./LndrGBP.sol');
const LndrHKD = artifacts.require('./LndrHKD.sol');
const LndrNOK = artifacts.require('./LndrNOK.sol');
const LndrNZD = artifacts.require('./LndrNZD.sol');
const LndrSEK = artifacts.require('./LndrSEK.sol');
const LndrIDR = artifacts.require('./LndrIDR.sol');
const LndrMYR = artifacts.require('./LndrMYR.sol');
const LndrSGD = artifacts.require('./LndrSGD.sol');
const LndrTHB = artifacts.require('./LndrTHB.sol');
const LndrVND = artifacts.require('./LndrVND.sol');
const LndrILS = artifacts.require('./LndrILS.sol');
const LndrRUB = artifacts.require('./LndrRUB.sol');
const LndrTRY = artifacts.require('./LndrTRY.sol');

module.exports = async function (deployer, network, accounts) {
    if (network == "gethtest" || network == "testrpc") {
        const usd = web3.fromAscii("USD");
        const jpy = web3.fromAscii("JPY");
        const krw = web3.fromAscii("KRW");
        const aud = web3.fromAscii("AUD");
        const cad = web3.fromAscii("CAD");
        const chf = web3.fromAscii("CHF");
        const cny = web3.fromAscii("CNY");
        const dkk = web3.fromAscii("DKK");
        const eur = web3.fromAscii("EUR");
        const gbp = web3.fromAscii("GBP");
        const hkd = web3.fromAscii("HKD");
        const nok = web3.fromAscii("NOK");
        const nzd = web3.fromAscii("NZD");
        const sek = web3.fromAscii("SEK");
        const idr = web3.fromAscii("IDR");
        const myr = web3.fromAscii("MYR");
        const sgd = web3.fromAscii("SGD");
        const thb = web3.fromAscii("THB");
        const vnd = web3.fromAscii("VND");
        const ils = web3.fromAscii("ILS");
        const rub = web3.fromAscii("RUB");
        const Try = web3.fromAscii("TRY");
        const mintAmount = web3.toBigNumber(web3.toWei(20000))
        var cpTokenContract;
        var creditProtocolContract;
        var lndrUsdContract;
        var lndrJpyContract;
        var lndrKrwContract;
        var lndrAudContract;
        var lndrCadContract;
        var lndrChfContract;
        var lndrCnyContract;
        var lndrDkkContract;
        var lndrEurContract;
        var lndrGbpContract;
        var lndrHkdContract;
        var lndrNokContract;
        var lndrNzdContract;
        var lndrSekContract;
        var lndrIdrContract;
        var lndrMyrContract;
        var lndrSgdContract;
        var lndrThbContract;
        var lndrVndContract;
        var lndrIlsContract;
        var lndrRubContract;
        var lndrTryContract;

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
        await deployer.deploy(LndrAUD, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrCAD, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrCHF, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrCNY, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrDKK, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrEUR, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrGBP, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrHKD, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrNOK, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrNZD, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrSEK, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrIDR, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrMYR, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrSGD, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrTHB, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrVND, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrILS, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrRUB, {from: web3.eth.accounts[0]});
        await deployer.deploy(LndrTRY, {from: web3.eth.accounts[0]});
        lndrUsdContract = await Lndr.deployed();
        lndrJpyContract = await LndrJPY.deployed();
        lndrKrwContract = await LndrKRW.deployed();
        lndrAudContract = await LndrAUD.deployed();
        lndrCadContract = await LndrCAD.deployed();
        lndrChfContract = await LndrCHF.deployed();
        lndrChfContract = await LndrCNY.deployed();
        lndrDkkContract = await LndrDKK.deployed();
        lndrEurContract = await LndrEUR.deployed();
        lndrGbpContract = await LndrGBP.deployed();
        lndrHkdContract = await LndrHKD.deployed();
        lndrNokContract = await LndrNOK.deployed();
        lndrNzdContract = await LndrNZD.deployed();
        lndrSekContract = await LndrSEK.deployed();
        lndrIdrContract = await LndrIDR.deployed();
        lndrMyrContract = await LndrMYR.deployed();
        lndrSgdContract = await LndrSGD.deployed();
        lndrThbContract = await LndrTHB.deployed();
        lndrVndContract = await LndrVND.deployed();
        lndrIlsContract = await LndrILS.deployed();
        lndrRubContract = await LndrRUB.deployed();
        lndrTryContract = await LndrTRY.deployed();
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
        await creditProtocolContract.createAndStakeUcac( lndrAudContract.address
                                                       , aud
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrCadContract.address
                                                       , cad
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrChfContract.address
                                                       , chf
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrCnyContract.address
                                                       , cny
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrDkkContract.address
                                                       , dkk
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrEurContract.address
                                                       , eur
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrGbpContract.address
                                                       , gbp
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrHkdContract.address
                                                       , hkd
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrNokContract.address
                                                       , nok
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrNzdContract.address
                                                       , nzd
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrSekContract.address
                                                       , sek
                                                       , web3.toWei(100)
                                                       , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrIdrContract.address
                                                        , idr
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrMyrContract.address
                                                        , myr
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrSgdContract.address
                                                        , sgd
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrThbContract.address
                                                        , thb
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrVndContract.address
                                                        , vnd
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrIlsContract.address
                                                        , ils
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrRubContract.address
                                                        , rub
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
        await creditProtocolContract.createAndStakeUcac( lndrTryContract.address
                                                        , Try
                                                        , web3.toWei(100)
                                                        , {from: web3.eth.accounts[0]});
    }
};
