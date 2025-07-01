-- Patch SQL
-- Revision: 19 -> 20

-- Drop indexes for crash_reports table
DROP INDEX IF EXISTS cr_app_idx;
DROP INDEX IF EXISTS cr_acb_complex_idx;
DROP INDEX IF EXISTS cr_acbv_complex_idx;
DROP INDEX IF EXISTS cr_game_engine_idx;
DROP INDEX IF EXISTS cr_username_idx;
DROP INDEX IF EXISTS cr_data_gin_idx;
DROP INDEX IF EXISTS cr_to_tsvector_idx;
DROP INDEX IF EXISTS cr_created_at_idx;
