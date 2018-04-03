# lndr-cli

LNDR client-server-blockchain integration tests are stored in `test/Spec.hs`.

## `lndr` Help Docs

```
lndr --help
lndr v0.1

lndr [COMMAND] ... [OPTIONS]
  Lend and borrow money.
  Server URL, default user, and default ucac must be indicated in configuration
  file.

Commands:
  transactions        List all transactions involving default user in default
                      Lndr UCAC
  pending             List all pending transactions
  rejectpending       Start interactive credit rejection procss
  lend                Submit a unilateral transaction as a creditor
  borrow              Submit a unilateral transaction as a debtor
  nick                Set a nickname for default user
  searchnick          Find address for a corresponding nickname
  getnonce            Display nonce between default user and the indicated
                      counterpary addess
  addfriend           Display nonce between default user and the indicated
                      counterpary addess
  removefriend        Remove a friend with the indicated address from the
                      default user's friend list
  setphoto            Use a particular image file as the default user's
                      profile photo
  unsubmitted         Prints txs that are in lndr db but not yet on the
                      blockchain
  info                Prints config, nick, and friends
  pendingsettlements  List all pending settlements
  lndrconfig          Prints config endpoint response

Common flags:
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```
