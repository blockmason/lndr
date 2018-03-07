#!/usr/bin/env bash

# This script assumes that psql db called ${dbname} exists and is accessible by ${dbuser}.

dbname="lndrtest"
dbuser="test"

function cleanup {
    kill -9 $ganache_pid
    kill -9 $lndr_server_pid
    sleep 1
    dropdb -U ${dbuser} ${dbname}
}

trap cleanup EXIT
cp ../lndr-backend/data/lndr-server.config.test ../lndr-backend/data/lndr-server.config

ganache_pid=`npm run ganache`
echo "Started ganache, pid ${ganache_pid}"

stack install --allow-different-user

npm run migrate

createdb -U ${dbuser} ${dbname} && psql -U ${dbuser} ${dbname} -f ../lndr-backend/db/create_tables.sql

cd ..

lndr-server &
lndr_server_pid=$!
echo "Started lndr-server, pid ${ganache_pid}"

stack test --allow-different-user
