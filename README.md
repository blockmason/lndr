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
./gethtest.sh &

# install ghc and fid-server application
#...TODO to be continued.....
```
