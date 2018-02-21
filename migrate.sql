alter table pending_credits
    add column ucac       char(40),
    add column created_at timestamp default now();

UPDATE pending_credits SET ucac = '869a8f2c3d22be392618ed06c8f548d1d5b5aed6';

alter table verified_credits
    add column ucac       char(40),
    add column created_at timestamp default now();

UPDATE verified_credits SET ucac = '869a8f2c3d22be392618ed06c8f548d1d5b5aed6';

alter table friendships
    add column created_at timestamp default now();

alter table nicknames
    add column created_at timestamp default now();
