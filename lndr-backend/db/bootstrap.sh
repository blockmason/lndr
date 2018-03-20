#!/usr/bin/env bash

BASE_DIR="$(cd "$(dirname "$0")"; pwd)"

# Set default values wherever necessary
[ -n "${DELAY}" ] || DELAY="0"
[ -n "${DB_HOST}" ] || DB_HOST="localhost"
[ -n "${DB_PORT}" ] || DB_PORT="5432"
[ -n "${DB_USERNAME}" ] || DB_USERNAME="lndr"
[ -n "${DB_PASSWORD}" ] || DB_PASSWORD="lndr"
[ -n "${DB_SCHEMA_FILE}" ] || DB_SCHEMA_FILE="${BASE_DIR}/create_tables.sql"

# Grace period to allow for the database to come online
sleep "${DELAY}"

# Seed the database with the schema file
echo "${DB_PASSWORD}" \
| psql \
  --host="${DB_HOST}" \
  --port="${DB_PORT}" \
  --username="${DB_USERNAME}" \
  --password \
  --file="${DB_SCHEMA_FILE}"
