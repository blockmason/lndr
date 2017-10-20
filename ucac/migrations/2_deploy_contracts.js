const CreditProtocol = artifacts.require('credit-protocol/contracts/CreditProtocol.sol');
const CPToken = artifacts.require('tce-contracts/contracts/CPToken.sol');
const FriendInDebt = artifacts.require('./FriendInDebt.sol');

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

module.exports = function(deployer, network, accounts) {
    if (network == "gethtest") {
        const usd = web3.fromAscii("USD");
        const ucacId1 = web3.sha3("hi");
        const ucacId2 = web3.sha3("yo");
        const creationStake = web3.toBigNumber(web3.toWei(3500));
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
            return deployer.deploy(FriendInDebt, {from: web3.eth.accounts[0]});
        }).then(() => {
            return FriendInDebt.deployed();
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
            return cpTokenContract.approve(creditProtocolContract.address, web3.toWei(1), {from: web3.eth.accounts[0]});
        }).then(() => {
            return creditProtocolContract.createAndStakeUcac( fidContract.address
                                                            , ucacId1
                                                            , usd
                                                            , web3.toWei(1)
                                                            , {from: web3.eth.accounts[0]});
        }).then(() => {
            return makeTransaction( creditProtocolContract, ucacId1, web3.eth.accounts[0]
                                  , web3.eth.accounts[1], web3.toBigNumber(10));
        }).catch(function(e) {
            console.log(e);
        });
        // deployer.deploy(CreditProtocol, this.cpToken.address
        // deployer.deploy(FriendInDebt);
        // this.cpToken = await CPToken.new({from: admin});
        // this.creditProtocol =
        //     await CreditProtocol.new( this.cpToken.address
        //                             , web3.toBigNumber(2 * 10 ** 9)
        //                             , web3.toBigNumber(1)
        //                             , {from: admin});
        // deployer.deploy(CreditProtocol);
        // this.basicUCAC = await BasicUCAC.new({from: admin});

        // await this.cpToken.mint(admin, web3.toWei(20000));
        // await this.cpToken.mint(p1, web3.toWei(20000));
        // await this.cpToken.mint(p2, web3.toWei(20000));
        // await this.cpToken.finishMinting();
        // await this.cpToken.endSale();

        // await this.cpToken.approve(this.creditProtocol.address, web3.toWei(1), {from: p1}).should.be.fulfilled;
        // await this.creditProtocol.createAndStakeUcac( this.basicUCAC.address
        //                                    , ucacId1, usd, web3.toWei(1), {from: p1}).should.be.fulfilled;

        // await makeTransaction(this.creditProtocol, ucacId1, p1, p2, web3.toBigNumber(10));
        // await makeTransaction(this.creditProtocol, ucacId1, p1, p2, web3.toBigNumber(10));
    }
};
