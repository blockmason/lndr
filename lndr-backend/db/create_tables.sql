-- todo make a alias for address char(20)

CREATE TABLE pending_credits (
    hash char(64) PRIMARY KEY,
    submitter char(40),
    nonce int,
    creditor char(40),
    debtor char(40),
    amount int,
    memo char(64),
    signature char(128)
);

CREATE TABLE friendships (
    id int PRIMARY KEY,
    origin char(40),
    friend char(40),
    unique (origin, friend)
);

CREATE TABLE nicknames (
    address char(40) PRIMARY KEY,
    nickname text UNIQUE
);
