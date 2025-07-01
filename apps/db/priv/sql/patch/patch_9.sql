-- Patch SQL
-- Revision: 8 -> 9

-- Cleanup crash_reports table
TRUNCATE TABLE crash_reports;

-- Add component column to crash_reports table
ALTER TABLE crash_reports ADD COLUMN component varchar NOT NULL;

-- Drop deprecated indexes
DROP INDEX IF EXISTS cr_app_branch_idx;
DROP INDEX IF EXISTS cr_app_branch_version_idx;

-- Add new indexes
CREATE INDEX cr_acb_complex_idx ON crash_reports (app, component, branch);
CREATE INDEX cr_acbv_complex_idx ON crash_reports (app, component, branch, version);
