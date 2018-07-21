CREATE TABLE paypal_requests (
    friend                      CHAR(40) PRIMARY KEY,
    requestor                   CHAR(40),
    created_at                  TIMESTAMP DEFAULT now()
);
