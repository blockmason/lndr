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
  id bigint,
  first varchar(100),
  middle varchar(100),
  last varchar(100),
  line1 varchar(200),
  line2 varchar(200),
  state varchar(4),
  city varchar (100),
  zip varchar (20),
  lat double precision,
  lng double precision,
  primary key (id)
);
