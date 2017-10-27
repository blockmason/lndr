# fid-cli

## Example usage

```
```

## `fiddy` Help Docs

```
stack exec -- fiddy --help
fiddy v0.1

fiddy [COMMAND] ... [OPTIONS]
  Lend and borrow money

Common flags:
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

fiddy transactions [OPTIONS]
  list all transactions processed by FiD UCAC

fiddy pending [OPTIONS]
  list all pending transactions

fiddy lend [OPTIONS]
  submit a unilateral transaction as a creditor

  -m --me=ITEM
  -f --friend=ITEM
  -a --amount=INT

fiddy borrow [OPTIONS]
  submit a unilateral transaction as a debtor

  -m --me=ITEM
  -f --friend=ITEM
  -a --amount=INT
```

## TODO

- sign transactions instead of depending on server to do it
- get friends lists from server
- simplify transactions submission
- better error handling
    + validate all input data
