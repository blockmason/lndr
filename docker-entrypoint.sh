#!/usr/bin/env bash

CONFIGURATION_FILE="${LNDR_HOME}/lndr-backend/data/lndr-server.config"
VARIABLE_NAMES="AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY CREDIT_PROTOCOL_ADDRESS DB_HOST DB_NAME DB_PASSWORD DB_PORT DB_USER ETHEREUM_PRIVATE_KEY ETHEREUM_CLIENT_URL ETHEREUM_GAS_PRICE ETHEREUM_MAX_GAS HEARTBEAT_INTERVAL ISSUE_CREDIT_EVENT LNDR_UCAC_AUD LNDR_UCAC_CAD LNDR_UCAC_CHF LNDR_UCAC_CNY LNDR_UCAC_DKK LNDR_UCAC_EUR LNDR_UCAC_GBP LNDR_UCAC_HKD LNDR_UCAC_IDR LNDR_UCAC_ILS LNDR_UCAC_INR LNDR_UCAC_JPY LNDR_UCAC_KRW LNDR_UCAC_MYR LNDR_UCAC_NOK LNDR_UCAC_NZD LNDR_UCAC_PLN LNDR_UCAC_RUB LNDR_UCAC_SEK LNDR_UCAC_SGD LNDR_UCAC_THB LNDR_UCAC_TRY LNDR_UCAC_USD LNDR_UCAC_VND S3_PHOTO_BUCKET SCAN_START_BLOCK NOTIFICATIONS_API_URL NOTIFICATIONS_API_KEY LNDR_BIND_ADDRESS LNDR_BIND_PORT"

lndr_config() {
  cp "${CONFIGURATION_FILE}.j2" "${CONFIGURATION_FILE}"
  for VARIABLE_NAME in ${VARIABLE_NAMES}; do
    sed -i'' "s|{{${VARIABLE_NAME}}}|${!VARIABLE_NAME}|g" "${CONFIGURATION_FILE}"
  done
}

lndr_inspect() {
  lndr_config
  echo "____BEGIN CONFIGURATION FILE_____"
  cat "${CONFIGURATION_FILE}"
  echo "____END CONFIGURATION FILE_____"
}

lndr_start() {
  lndr_config
  lndr-server
}

lndr_delay() {
  local DELAY="$1"
  [ -n "${DELAY}" ] || DELAY="5"
  sleep "${DELAY}"
}

case "$1" in
  start)
    lndr_start
    ;;
  delay-start)
    lndr_delay && lndr_start
    ;;
  inspect)
    lndr_inspect
    ;;
  *)
    exec $*
    ;;
esac
