pragma solidity ^0.4.24;

contract LndrVND {
    uint constant decimals = 0;

    function allowTransaction(address, address, uint256) public pure returns (bool) {
        return true;
    }
}
