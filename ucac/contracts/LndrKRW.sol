pragma solidity ^0.4.24;

contract LndrKRW {
    function allowTransaction(address, address, uint256) public pure returns (bool) {
        return true;
    }
}
