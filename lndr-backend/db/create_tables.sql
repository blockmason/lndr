CREATE TABLE pending_credits (
    hash        CHAR(64) PRIMARY KEY,
    submitter   CHAR(40),
    nonce       NUMERIC(78),
    creditor    CHAR(40),
    debtor      CHAR(40),
    amount      NUMERIC(78),
    memo        CHAR(32),
    signature   CHAR(130),
    ucac        CHAR(40)
);

CREATE TABLE verified_credits (
    hash               CHAR(64) PRIMARY KEY,
    nonce              NUMERIC(78),
    creditor           CHAR(40),
    debtor             CHAR(40),
    amount             NUMERIC(78),
    memo               CHAR(32),
    creditor_signature CHAR(130),
    debtor_signature   CHAR(130),
    ucac               CHAR(40)
);

CREATE TABLE settlements (
    hash               CHAR(64) PRIMARY KEY,
    amount             NUMERIC(78) NOT NULL,
    currency           CHAR(3) NOT NULL,
    blocknumber        NUMERIC(78) NOT NULL,
    verified           BOOLEAN NOT NULL,
    tx_hash            CHAR(64),
    created_at         TIMESTAMP DEFAULT now() NOT NULL
);

CREATE TABLE friendships (
    id     SERIAL PRIMARY KEY,
    origin CHAR(40),
    friend CHAR(40),
    UNIQUE (origin, friend)
);

CREATE TABLE nicknames (
    address     CHAR(40) PRIMARY KEY,
    nickname    TEXT UNIQUE,
    email       TEXT UNIQUE
);

CREATE TABLE push_data (
    address     CHAR(40) PRIMARY KEY,
    channel_id  TEXT UNIQUE,
    platform    TEXT
);
