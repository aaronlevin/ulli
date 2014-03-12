BEGIN;

-- TODO: everything

CREATE TABLE list (
  id BIGSERIAL NOT NULL PRIMARY KEY,
  label TEXT NULL,
  description TEXT NULL
);

CREATE TABLE list_item (
  id BIGSERIAL NOT NULL PRIMARY KEY,
  list_id BIGINT NOT NULL REFERENCES list(id),
  label TEXT NULL,
  url TEXT NULL
);

COMMIT;

