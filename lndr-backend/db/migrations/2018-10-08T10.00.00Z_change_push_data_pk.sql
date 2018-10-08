ALTER TABLE push_data ADD COLUMN new_channel_id CHAR(36);
UPDATE push_data SET new_channel_id = channel_id;
ALTER TABLE push_data DROP COLUMN channel_id;
ALTER TABLE push_data RENAME COLUMN new_channel_id TO channel_id;
ALTER TABLE push_data DROP CONSTRAINT push_data_pkey;
ALTER TABLE push_data ADD PRIMARY KEY (channel_id);
