-- todo make a alias for address char(20)

CREATE TABLE pending_credits (
    hash char(32) PRIMARY KEY,
    submitter char(20),
    nonce int,
    creditor char(20),
    debtor char(20),
    amount int,
    memo char(32),
    signature char(64)
);

CREATE TABLE friendships (
    id int PRIMARY KEY,
    origin char(20),
    friend char(20)
);

CREATE TABLE nicknames (
    address char(20) PRIMARY KEY,
    nickname text
);
