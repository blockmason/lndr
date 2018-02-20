FROM debian:stretch

RUN set -e;\
  apt-get update;\
  apt-get install curl libpq-dev -y;\
  apt-get --purge autoremove;\
  apt-get clean;

ENV \
  LNDR_HOME=/lndr \
  CIRCLECI_PROJECT_ID=107488794 \
  CIRCLECI_BUILD_NUMBER=8

RUN set -e;\
  curl -sSL -o /usr/bin/lndr-server "https://${CIRCLECI_BUILD_NUMBER}-${CIRCLECI_PROJECT_ID}-gh.circle-artifacts.com/0/root/project/artifacts/lndr-server";\
  chmod 0555 /usr/bin/lndr-server;

COPY . "${LNDR_HOME}"

WORKDIR "${LNDR_HOME}"

ENV \
  AWS_ACCESS_KEY_ID="" \
  AWS_SECRET_ACCESS_KEY="" \
  CREDIT_PROTOCOL_ADDRESS="0xd5ec73eac35fc9dd6c3f440bce314779fed09f60" \
  DB_HOST="127.0.0.1" \
  DB_NAME="lndr" \
  DB_PASSWORD="" \
  DB_PORT="5432" \
  DB_USER="" \
  ETHEREUM_ACCOUNT="0x198e13017d2333712bd942d8b028610b95c363da" \
  ETHEREUM_CLIENT_URL="http://127.0.0.1:8545" \
  ETHEREUM_GAS_PRICE="200000000" \
  ETHEREUM_MAX_GAS="250000" \
  HEARTBEAT_INTERVAL="5" \
  ISSUE_CREDIT_EVENT="0xcbc85a9af1e8adce13cbeff2e71299b0f3243d7ef1eaec93a9a281e939aceb7b" \
  LNDR_UCAC_JPY="0x9d9462f70067f506ac26bd523222f4f8020924d4" \
  LNDR_UCAC_KRW="0x9945a5b005a898a435adf30fe88f2818ccc0ba5c" \
  LNDR_UCAC_USD="0x7899b83071d9704af0b132859a04bb1698a3acaf" \
  S3_PHOTO_BUCKET="lndr-avatars" \
  SCAN_START_BLOCK="0" \
  URBAN_AIRSHIP_KEY="" \
  URBAN_AIRSHIP_SECRET=""

ENTRYPOINT ["./docker-entrypoint.sh"]
CMD ["start"]
