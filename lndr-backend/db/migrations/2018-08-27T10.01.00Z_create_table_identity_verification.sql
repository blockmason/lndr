CREATE TABLE identity_verification (
    address         CHAR(40) PRIMARY KEY,
    applicant_id    CHAR(40),
    status          CHAR(40),
    created_at      TIMESTAMP DEFAULT now()
);
