ALTER TABLE push_data DROP CONSTRAINT push_data_pkey;
ALTER TABLE push_data ADD PRIMARY KEY (channel_id);
