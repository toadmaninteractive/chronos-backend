-- Patch SQL
-- Revision: 21 -> 22

-- Add acbv_quad generated column to crash_reports table
ALTER TABLE crash_reports ADD COLUMN IF NOT EXISTS acbv_quad text GENERATED ALWAYS AS (app || '\t' || component || '\t' || branch || '\t' || version) STORED;

-- Re-create indexes for crash_reports table
CREATE INDEX crash_reports_acbv_quad_idx ON crash_reports USING BTREE(acbv_quad);
CREATE INDEX crash_reports_acb_ts_idx ON crash_reports USING BTREE(app, component, branch, created_at DESC);
CREATE INDEX crash_reports_acbv_ts_idx ON crash_reports USING BTREE(app, component, branch, version, created_at DESC);
CREATE INDEX crash_reports_acbvr_ts_trgm_gin_idx ON crash_reports USING GIN (app, component, branch, version, created_at, reason gin_trgm_ops);
