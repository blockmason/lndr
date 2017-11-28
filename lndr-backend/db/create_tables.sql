CREATE TABLE pending_credits (
    hash        CHAR(64) PRIMARY KEY,
    submitter   CHAR(40),
    nonce       INT,
    creditor    CHAR(40),
    debtor      CHAR(40),
    amount      INT,
    memo        CHAR(64),
    signature   CHAR(130)
);

CREATE TABLE verified_credits (
    hash               CHAR(64) PRIMARY KEY,
    submitter          CHAR(40),
    nonce              INT,
    creditor           CHAR(40),
    debtor             CHAR(40),
    amount             INT,
    memo               CHAR(64),
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
