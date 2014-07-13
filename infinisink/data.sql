DROP TYPE IF EXISTS e_medium;
CREATE TYPE e_medium AS ENUM ('twitter','email','facebook','rest','sms','bookmark');

DROP TABLE IF EXISTS account CASCADE;
CREATE TABLE account (
    id      bigserial PRIMARY KEY,
    email   text
);

DROP TABLE IF EXISTS application CASCADE;
CREATE TABLE application (
    id bigserial PRIMARY KEY,
    name text,
    slug text NOT NULL,
    CONSTRAINT app_slug UNIQUE(slug)
);

DROP TABLE IF EXISTS account_application CASCADE;
CREATE TABLE account_application (
    account bigint references account (id) ON UPDATE CASCADE ON DELETE CASCADE,
    application bigint references application (id) ON UPDATE CASCADE,
    CONSTRAINT account_application_pkey PRIMARY KEY (account, application)
);

DROP TABLE IF EXISTS sink_message CASCADE;
CREATE TABLE sink_message (
    id      bigserial PRIMARY KEY,
    medium  e_medium NOT NULL,
    message text NOT NULL,
    payload text,
    timestamp integer NOT NULL,
    application text references application (slug) ON UPDATE CASCADE ON DELETE CASCADE
);
