# fid-cli

## Example usage

```
λ fiddy transactions

[]

λ fiddy pending

[]

λ fiddy borrow --me=0x1ab560ad22f10d0882c31e57240d6b7ac0b42d48 --fri end=0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb --amount=45

SubmissionResponse
    { hash = "0x8d2bce94f02a4cfde797b377802d6a28e1398ea80aa48f0af0f448b5a67fd072"
    , nonce = 0
    }

λ fiddy transactions

[]

λ fiddy pending

[
    ( "0x8d2bce94f02a4cfde797b377802d6a28e1398ea80aa48f0af0f448b5a67fd072"
    , CreditRecord
        { creditor = "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
        , debtor = "0x1ab560ad22f10d0882c31e57240d6b7ac0b42d48"
        , amount = 45
        , signature = "0xe474fe2856ca9804edffd890de63ab75752cce19bcf27bd550d2a0bd041ef4873ff06da574196b0e0d5bb5e3315a6ce2b6dde3bbde7755c6de27f6a79e4c822c1c"
        }
    )
]

λ fiddy lend --me=0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb --friend=0x1ab560ad22f10d0882c31e57240d6b7ac0b42d48 --amount=45

SubmissionResponse
    { hash = "0x8d2bce94f02a4cfde797b377802d6a28e1398ea80aa48f0af0f448b5a67fd072"
    , nonce = 0
    }

λ fiddy pending

[]

λ fiddy transactions
[ IssueCreditLog
    { ucac = d5ec73eac35fc9dd6c3f440bce314779fed09f60
    , creditor = 6a362e5cee1cf5a5408ff1e12b0bc546618dffcb
    , debtor = 1ab560ad22f10d0882c31e57240d6b7ac0b42d48
    , amount = 45
    }
]
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
