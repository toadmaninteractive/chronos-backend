-- Patch SQL
-- Revision: 12 -> 13

-- Personnel accounts
CREATE TABLE personnel
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    username varchar UNIQUE NOT NULL CHECK (TRIM(username) <> ''),
    name varchar DEFAULT NULL,
    email varchar DEFAULT NULL CHECK (TRIM(email) <> ''),
    phone varchar DEFAULT NULL CHECK (TRIM(phone) <> ''),
    is_blocked boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX per_username_ult_index ON personnel (LOWER(TRIM(username)));
CREATE UNIQUE INDEX per_email_ult_index ON personnel (LOWER(TRIM(email)));
CREATE UNIQUE INDEX per_phone_ult_index ON personnel (LOWER(TRIM(phone)));
CREATE INDEX per_is_blocked_index ON personnel (is_blocked);
CREATE INDEX per_is_deleted_index ON personnel (is_deleted);
CREATE INDEX per_created_at_index ON personnel (created_at);
CREATE INDEX per_updated_at_index ON personnel (updated_at);

-- Personnel sessions
CREATE TABLE personnel_sessions
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    valid_thru timestamptz NOT NULL
) WITHOUT OIDS;

CREATE INDEX pers_personnel_id_index ON personnel_sessions (personnel_id);
CREATE INDEX pers_created_at_index ON personnel_sessions (created_at);
CREATE INDEX pers_valid_thru_index ON personnel_sessions (valid_thru);
