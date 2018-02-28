#!/usr/bin/env bash

function cleanup {
    kill -9 $ganache_pid
    kill -9 $lndr_server_pid
    sleep 1
    dropdb -U aupiff lndr
}

trap cleanup EXIT

ganache-cli -b 1 -e 100000000000000000000 -m gravity top burden flip student usage spell purchase hundred improve check genre > /dev/null &
ganache_pid=$!
echo "Started ganache, pid ${ganache_pid}"

sleep 1

stack install --allow-different-user

npm run migrate

createdb -U aupiff lndr && psql -U aupiff lndr -f ../lndr-backend/db/create_tables.sql

cd ..

lndr-server &
lndr_server_pid=$!
echo "Started lndr-server, pid ${ganache_pid}"

sleep 3

ab -n 100 -c 10 http://127.0.0.1:80/config > test1.txt
ab -n 100 -c 10 http://127.0.0.1:80/transactions > test2.txt
