-- Patch SQL
-- Revision: 23 -> 24

-- Add user_comment column to crash_reports table
ALTER TABLE crash_reports ADD COLUMN user_comment text;

-- Add new index
CREATE INDEX crash_reports_acbvuc_ts_trgm_gin_idx ON crash_reports USING GIN (app, component, branch, version, created_at, user_comment gin_trgm_ops);
