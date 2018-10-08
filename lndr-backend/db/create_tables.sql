CREATE TABLE pending_credits (
    hash                CHAR(64) PRIMARY KEY,
    submitter           CHAR(40),
    nonce               NUMERIC(78),
    creditor            CHAR(40),
    debtor              CHAR(40),
    amount              NUMERIC(78),
    memo                CHAR(32),
    signature           CHAR(130),
    ucac                CHAR(40),
    settlement_currency CHAR(10),
    created_at          TIMESTAMP DEFAULT now()
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
    ucac               CHAR(40),
    created_at         TIMESTAMP DEFAULT now(),
    submitter          CHAR(40)
);

CREATE TABLE settlements (
    hash               CHAR(64) PRIMARY KEY,
    amount             NUMERIC(78) NOT NULL,
    currency           CHAR(3) NOT NULL,
    blocknumber        NUMERIC(78) NOT NULL,
    verified           BOOLEAN NOT NULL,
    tx_hash            CHAR(64),
    created_at         TIMESTAMP DEFAULT now()
);

CREATE TABLE friendships (
    id          SERIAL PRIMARY KEY,
    origin      CHAR(40),
    friend      CHAR(40),
    created_at  TIMESTAMP DEFAULT now(),
    UNIQUE (origin, friend)
);

CREATE TABLE nicknames (
    address     CHAR(40) PRIMARY KEY,
    nickname    TEXT UNIQUE,
    email       TEXT UNIQUE,
    created_at  TIMESTAMP DEFAULT now()
);

CREATE TABLE push_data (
    channel_id  CHAR(36) PRIMARY KEY,
    address     CHAR(40),
    platform    TEXT
);

CREATE TABLE paypal_requests (
    friend      CHAR(40),
    requestor   CHAR(40),
    created_at  TIMESTAMP DEFAULT now(),
    PRIMARY KEY (friend, requestor)
);

CREATE TABLE identity_verification (
    address         CHAR(40) PRIMARY KEY,
    applicant_id    CHAR(40),
    status          CHAR(40),
    created_at      TIMESTAMP DEFAULT now()
);
