pragma solidity 0.4.15;

// TODO this import is only here to make sure there is a compiled
// CreditProtocol.sol available to truffle when I run tests. There is likely
// a workaround for this.
import "credit-protocol/contracts/CreditProtocol.sol";

contract FriendInDebt {
    function allowTransaction(address creditor, address debtor, uint256 amount) public returns (bool) {
        return true;
    }
}
