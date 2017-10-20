#!/bin/bash
function cleanup {
    kill -9 $geth_pid
    rm -rf geth
}

trap cleanup EXIT

rm -rf build
mkdir geth && mkdir geth/privchain
cat << EOF > geth/genesis.json
{
    "config": {
        "chainId": 88888,
        "homesteadBlock": 0,
        "eip155Block": 0,
        "eip158Block": 0
    },
    "coinbase" : "0x0000000000000000000000000000000000000000",
    "difficulty" : "0x1",
    "extraData" : "0x00",
    "gasLimit" : "0x47e7c5",
    "nonce" : "0x0000000000000042",
    "mixhash" : "0x0000000000000000000000000000000000000000000000000000000000000000",
    "parentHash" : "0x0000000000000000000000000000000000000000000000000000000000000000",
    "timestamp" : "0x00",
    "alloc" : {
        "198e13017d2333712bd942d8b028610b95c363da": {"balance": "888888888888888888888888"},

        "8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf": {"balance": "888888888888888888888888"},
        "1ba7167373f13d28cc112f373bac8d5a07a47af9": {"balance": "888888888888888888888888"},
        "1ab560ad22f10d0882c31e57240d6b7ac0b42d48": {"balance": "888888888888888888888888"},
        "bc967b50ec58f4ba0d6221207fac50d38274d476": {"balance": "888888888888888888888888"},
        "11edd217a875063583dd1b638d16810c5d34d54b": {"balance": "888888888888888888888888"},
        "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb": {"balance": "888888888888888888888888"},
        "0f2f2966fc0050d5a6a3fd9d324f60148e06fb19": {"balance": "888888888888888888888888"},
        "2a91f48f0e84317d8dfaf939fea487031acbc46b": {"balance": "888888888888888888888888"}
    }
}
EOF

privkeys=("7231a774a538fce22a329729b03087de4cb4a1119494db1c10eae3bb491823e7" "b217205550c6011141e3580142ac43d7d41d217102f30e816eb36b70727e292e") # "f581608ccd4dcd78e341e464b86f268b77ee2673acc705023e64eeb5a4e31490" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea098" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea099" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea100" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea101" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea102" "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea103")

pubkeys=("198e13017d2333712bd942d8b028610b95c363da" "8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf") # "1ba7167373f13d28cc112f373bac8d5a07a47af9" "1ab560ad22f10d0882c31e57240d6b7ac0b42d48" "bc967b50ec58f4ba0d6221207fac50d38274d476" "11edd217a875063583dd1b638d16810c5d34d54b" "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb" "0f2f2966fc0050d5a6a3fd9d324f60148e06fb19" "2a91f48f0e84317d8dfaf939fea487031acbc46b")

geth --datadir geth/privchain init geth/genesis.json

geth --port 3001 --networkid 58342 --nodiscover --datadir="geth/privchain" --maxpeers=0 \
     --rpc --rpcport 8545 --rpcaddr 127.0.0.1 --rpccorsdomain "*" --rpcapi "eth,net,web3,personal" --mine --minerthreads=1 --etherbase "0x198e13017d2333712bd942d8b028610b95c363da" &
geth_pid=$!

sleep 1

for i in "${privkeys[@]}"
do
    geth --datadir geth/privchain --password <(echo "pass") account import <(echo $i)
done

sleep 1

a='{"jsonrpc":"2.0","method":"personal_unlockAccount","params":["'
c='", "pass", 0],"id":67}'
for i in "${pubkeys[@]}"
do
    b="0x$i"
    curl -X POST --data "$a$b$c" http://localhost:8545
done

truffle migrate --reset --network gethtest
wait
