# friend-in-debt

Server, CLI, and UCAC Solidity contracts.

Currently

pubkeys=("198e13017d2333712bd942d8b028610b95c363da" "8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf" "1ba7167373f13d28cc112f373bac8d5a07a47af9" "1ab560ad22f10d0882c31e57240d6b7ac0b42d48" "bc967b50ec58f4ba0d6221207fac50d38274d476" "11edd217a875063583dd1b638d16810c5d34d54b" "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb" "0f2f2966fc0050d5a6a3fd9d324f60148e06fb19" "2a91f48f0e84317d8dfaf939fea487031acbc46b")

## fid-backend

[fid-backend README](fid-backend/README.md)

## fid-cli

[fid-cli README](fid-cli/README.md)

```
stack install
```

```
stack exec -- fiddy send --amount 10 --config "../test/config2" --creditor=0x198e13017d2333712bd942d8b028610b95c363da
```

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
./gethtest.sh &

# install ghc and fid-server application

```
