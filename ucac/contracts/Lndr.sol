pragma solidity ^0.4.24;

// TODO this import is only here to make sure there is a compiled
// CreditProtocol.sol available to truffle when I run tests. There is likely
// a workaround for this.
import "credit-protocol/contracts/CreditProtocol.sol";

contract Lndr {
    function allowTransaction(address, address, uint256) public pure returns (bool) {
        return true;
    }
}
