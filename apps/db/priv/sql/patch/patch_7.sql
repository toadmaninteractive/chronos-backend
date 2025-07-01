-- Patch SQL
-- Revision: 6 -> 7

-- Crash reports
CREATE TABLE crash_reports
(
    id bigserial PRIMARY KEY,
    app varchar NOT NULL,
    branch varchar NOT NULL,
    version varchar NOT NULL,
    game_engine varchar DEFAULT NULL CHECK (TRIM(game_engine) <> ''),
    username varchar NOT NULL,
    filename varchar NOT NULL,
    reason varchar NOT NULL,
    data jsonb NOT NULL DEFAULT '{}',
    created_at timestamptz
) WITHOUT OIDS;

CREATE INDEX cr_app_idx ON crash_reports (app);
CREATE INDEX cr_app_branch_idx ON crash_reports (app, branch);
CREATE INDEX cr_app_branch_version_idx ON crash_reports (app, branch, version);
CREATE INDEX cr_game_engine_idx ON crash_reports (game_engine);
CREATE INDEX cr_username_idx ON crash_reports (username);
CREATE INDEX cr_data_gin_idx ON crash_reports USING gin(data);
CREATE INDEX cr_to_tsvector_idx ON crash_reports USING gin(to_tsvector('english', reason));
CREATE INDEX cr_created_at_idx ON crash_reports (created_at);
