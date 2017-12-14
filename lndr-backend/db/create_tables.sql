CREATE TABLE pending_credits (
    hash        CHAR(64) PRIMARY KEY,
    submitter   CHAR(40),
    nonce       BIGINT,
    creditor    CHAR(40),
    debtor      CHAR(40),
    amount      BIGINT,
    memo        CHAR(32),
    signature   CHAR(130)
);

CREATE TABLE verified_credits (
    hash               CHAR(64) PRIMARY KEY,
    nonce              BIGINT,
    creditor           CHAR(40),
    debtor             CHAR(40),
    amount             BIGINT,
    memo               CHAR(32),
    creditor_signature CHAR(130),
    debtor_signature   CHAR(130)
);

CREATE TABLE friendships (
    id     SERIAL PRIMARY KEY,
    origin CHAR(40),
    friend CHAR(40),
    UNIQUE (origin, friend)
);

CREATE TABLE nicknames (
    address     CHAR(40) PRIMARY KEY,
    nickname    TEXT UNIQUE
);

CREATE EXTENSION pg_trgm;
