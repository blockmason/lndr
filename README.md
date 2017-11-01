# friend-in-debt

Server, CLI, and UCAC Solidity contracts.

Currently, the server has access to the following accounts which are unlocked on a local blockchain; for more details, read [gethtest](ucac/gethtest.sh).

```
0x198e13017d2333712bd942d8b028610b95c363da
0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf
0x1ba7167373f13d28cc112f373bac8d5a07a47af9
0x1ab560ad22f10d0882c31e57240d6b7ac0b42d48
0xbc967b50ec58f4ba0d6221207fac50d38274d476
0x11edd217a875063583dd1b638d16810c5d34d54b
0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb
0x0f2f2966fc0050d5a6a3fd9d324f60148e06fb19
0x2a91f48f0e84317d8dfaf939fea487031acbc46b
```

The CLI application `fiddy` currently does no siging itself though this will
change in the near future.

## API

There are currently only three endpoints:

```
type API = "transactions" :> Get '[JSON] [IssueCreditLog]
      :<|> "pending" :> Get '[JSON] [(Text, CreditRecord Signed)]
      :<|> "submit" :> ReqBody '[JSON] (CreditRecord Unsigned) :> Post '[JSON] SubmissionResponse
```


```
GET /transactions

RESPONSE: (array of CreditRecords)
[ { "ucac": "d5ec73eac35fc9dd6c3f440bce314779fed09f60"
  , "creditor": "198e13017d2333712bd942d8b028610b95c363da"
  , "debtor": "8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
  , "amount": 123
  }
, { "ucac": "d5ec73eac35fc9dd6c3f440bce314779fed09f60"
  , "creditor": "11edd217a875063583dd1b638d16810c5d34d54b"
  , "debtor": "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
  , "amount": 69
  }
]
```

```
GET /pending

RESPONSE: Array of 2-element arrays containing a hash and a signed CreditRecord
[
   [ "0x7e2e9ff3a5fc148cf76261755c4c666630bfc3a28d02733cfbe721fc965aca28"
   , { "creditor": "0x11edd217a875063583dd1b638d16810c5d34d54b"
     , "debtor": "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
     , "amount": 69
     , "signature": "0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"
     }
   ]
   ,
   [ "0xe2ab6fe922237d44fbfd6b2302b06491c98b4f0bb3046ab9263c4cbfc889f07a"
   , { "creditor": "0x11edd217a875063583dd1b638d16810c5d34d54b"
     , "debtor": "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
     , "amount": 69
     , "signature": "0xaf169e172b3070139ca8ad7e806ccd1d4628af64ecfef41cd9a4ffda7660e3623e400daf5996ec08333042c6298d3ed729aa4217eb32e8a0a62763d214e0dd781b"
     }
   ]
]
```

```
POST /submit

**sort of hacky right now, must put signer address in the signature field**

REQUEST BODY:
{ "creditor": "0x11edd217a875063583dd1b638d16810c5d34d54b"
, "debtor": "0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
, "amount": 69
, "signature": "0x11edd217a875063583dd1b638d16810c5d34d54b"
}

RESPONSE:
{ hash = "0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"
, nonce = 1
}
```

## Install

Once you have [stack]() installed, run the following commands:

```
stack setup
stack build
stack install
```

For `stack install` to register binaries properly, you must have `.local/bin/`
on your `PATH`.

## fid-backend

[fid-backend README](fid-backend/README.md)

## fid-cli

[fid-cli README](fid-cli/README.md)

## UCAC contract

[FriendInDebt.sol README](ucac/README.md)

## deploy process

On AWS ubuntu server. Deploy will be automated soon.

```
# install stack
curl -sSL https://get.haskellstack.org/ | sh

# clone friend in debt repo
git clone http://github.com/blockmason/friend-in-debt.git

# install geth
sudo apt-get install software-properties-common
sudo add-apt-repository -y ppa:ethereum/ethereum
sudo apt-get update
sudo apt-get install ethereum

# start a local blockchain in the background
cd friend-in-debt/ucac
sudo apt-get install npm
sudo npm install -g truffle
npm install
./gethtest.sh &

# install ghc and fid-server application
cd friend-in-debt
stack setup
stack build
```
