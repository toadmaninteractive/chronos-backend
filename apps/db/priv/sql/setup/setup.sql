-- Setup SQL
-- Revision: 24

-----------------
-- Extensions  --
-----------------

CREATE EXTENSION btree_gin;
CREATE EXTENSION pg_trgm;

-------------------
-- Custom types  --
-------------------

CREATE TYPE loglevel AS ENUM (
    'trace',
    'debug',
    'info',
    'notice',
    'warning',
    'error',
    'fatal'
);

------------------------
-- Tables and indexes --
------------------------

-- Properties
CREATE TABLE props
(
    name varchar PRIMARY KEY CHECK (TRIM(name) <> ''),
    value varchar
) WITHOUT OIDS;

-- Settings
CREATE TABLE settings
(
    param varchar PRIMARY KEY CHECK (TRIM(param) <> ''),
    type varchar NOT NULL CHECK (TRIM(type) <> ''),
    value text DEFAULT NULL
) WITHOUT OIDS;

INSERT INTO settings ("param", "type", "value") VALUES 
    ('personnel_session_duration', 'int', '31536000'),
    ('api_key', 'string', NULL);

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

-- Logs
CREATE TABLE logs
(
    id bigserial PRIMARY KEY,
    app text NOT NULL,
    component text NOT NULL,
    branch text NOT NULL,
    version text NOT NULL DEFAULT 'None',
    timestamp timestamptz,
    level loglevel NOT NULL,
    data jsonb NOT NULL,
    message text NOT NULL,
    msg_count integer NOT NULL DEFAULT 1,
    seq_id integer NOT NULL DEFAULT 1,
    acbv_quad text GENERATED ALWAYS AS (app || '\t' || component || '\t' || branch || '\t' || version) STORED
) WITHOUT OIDS;

CREATE INDEX logs_acbv_quad_idx ON logs USING BTREE(acbv_quad);
CREATE INDEX logs_acb_tseq_idx ON logs USING BTREE(app, component, branch, "timestamp" DESC, seq_id DESC);
CREATE INDEX logs_acbv_tseq_idx ON logs USING BTREE(app, component, branch, version, "timestamp" DESC, seq_id DESC);
CREATE INDEX logs_acbvm_tseq_trgm_gin_idx ON logs USING GIN (app, component, branch, version, "timestamp", seq_id, message gin_trgm_ops);

CREATE OR REPLACE VIEW acbv_logs AS
    SELECT acbv[1] AS app, acbv[2] AS component, acbv[3] AS branch, acbv[4] AS version
    FROM (
        WITH RECURSIVE t AS (
            (SELECT acbv_quad FROM logs ORDER BY acbv_quad LIMIT 1)
            UNION ALL
            SELECT (SELECT acbv_quad FROM logs WHERE acbv_quad > t.acbv_quad ORDER BY acbv_quad LIMIT 1) FROM t
            WHERE t.acbv_quad IS NOT NULL
        )
        SELECT string_to_array(acbv_quad, '\t') AS acbv FROM t WHERE acbv_quad IS NOT NULL
    ) AS t1;

-- Crash reports
CREATE TABLE crash_reports
(
    id bigserial PRIMARY KEY,
    app text NOT NULL,
    component text NOT NULL,
    branch text NOT NULL,
    version text NOT NULL,
    game_engine text DEFAULT NULL CHECK (TRIM(game_engine) <> ''),
    username text NOT NULL,
    filename text NOT NULL,
    reason text NOT NULL,
    user_comment text,
    data jsonb NOT NULL DEFAULT '{}',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    acbv_quad text GENERATED ALWAYS AS (app || '\t' || component || '\t' || branch || '\t' || version) STORED
) WITHOUT OIDS;

CREATE INDEX crash_reports_acbv_quad_idx ON crash_reports USING BTREE(acbv_quad);
CREATE INDEX crash_reports_acb_ts_idx ON crash_reports USING BTREE(app, component, branch, created_at DESC);
CREATE INDEX crash_reports_acbv_ts_idx ON crash_reports USING BTREE(app, component, branch, version, created_at DESC);
CREATE INDEX crash_reports_acbvr_ts_trgm_gin_idx ON crash_reports USING GIN (app, component, branch, version, created_at, reason gin_trgm_ops);
CREATE INDEX crash_reports_acbvuc_ts_trgm_gin_idx ON crash_reports USING GIN (app, component, branch, version, created_at, user_comment gin_trgm_ops);

CREATE OR REPLACE VIEW acbv_crash_reports AS
    SELECT acbv[1] AS app, acbv[2] AS component, acbv[3] AS branch, acbv[4] AS version
    FROM (
        WITH RECURSIVE t AS (
            (SELECT acbv_quad FROM crash_reports ORDER BY acbv_quad LIMIT 1)
            UNION ALL
            SELECT (SELECT acbv_quad FROM crash_reports WHERE acbv_quad > t.acbv_quad ORDER BY acbv_quad LIMIT 1) FROM t
            WHERE t.acbv_quad IS NOT NULL
        )
        SELECT string_to_array(acbv_quad, '\t') AS acbv FROM t WHERE acbv_quad IS NOT NULL
    ) AS t1;
