ALTER TABLE paypal_requests DROP CONSTRAINT paypal_requests_pkey;
ALTER TABLE paypal_requests ADD PRIMARY KEY (friend, requestor);
