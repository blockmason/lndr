FROM debian:stretch

RUN set -e;\
  apt-get update;\
  apt-get install curl libpq-dev libtinfo-dev -y;\
  apt-get --purge autoremove;\
  apt-get clean;

ENV \
  LNDR_HOME=/lndr

COPY . "${LNDR_HOME}"

WORKDIR "${LNDR_HOME}"

RUN set -e;\
  [ -f "${LNDR_HOME}/lndr-server" ] && cp "${LNDR_HOME}/lndr-server" /usr/bin/lndr-server;\
  [ -f "/usr/bin/lndr-server" ] \
  || curl \
      -sSL \
      -o /usr/bin/lndr-server \
      $(\
        curl \
          -sSL \
          'https://circleci.com/api/v1.1/project/github/blockmason/lndr/latest/artifacts?branch=master&filter=successful' \
        | awk '/url.+lndr-server/ { print $3; }' \
        | xargs \
      );\
  chmod 0555 /usr/bin/lndr-server;

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
  LNDR_UCAC_AUD="0xb82acb57fcc7fec3438f02845dfdfad29a4589e4" \
  LNDR_UCAC_CAD="0xd75fcd5ca7b235eaeabc82fda406d7a87101bbc0" \
  LNDR_UCAC_CHF="0x519b1153e2c822e605d36ddb959ba233f658afd1" \
  LNDR_UCAC_CNY="0xe131b33e6a02ae1cd5152c3d2c2812188cdf7f4a" \
  LNDR_UCAC_DKK="0x061a0d4f6e7f71ed18263f188c2fd29945fdce7f" \
  LNDR_UCAC_EUR="0xae0d61120070411bf96b1d0d42fe9f4023e9f8f1" \
  LNDR_UCAC_GBP="0x5c433119ec13fdac42359838f7df93124d054d0c" \
  LNDR_UCAC_NOK="0x943b8e14145692f33082ca062ee48be49d48d476" \
  LNDR_UCAC_NZD="0x28a2997d3c21087053b7bb5c244bf06bf84c984e" \
  LNDR_UCAC_SEK="0x55b1d21c802e74b5e2230e9f3af28d22f8128ddd" \
  S3_PHOTO_BUCKET="lndr-avatars" \
  SCAN_START_BLOCK="0" \
  URBAN_AIRSHIP_KEY="" \
  URBAN_AIRSHIP_SECRET=""

ENTRYPOINT ["./docker-entrypoint.sh"]
CMD ["start"]
