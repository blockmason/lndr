pragma solidity 0.4.15;

contract LndrVND {
    uint constant decimals = 0;

    function allowTransaction(address creditor, address debtor, uint256 amount) public returns (bool) {
        return true;
    }
}
