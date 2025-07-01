-- Patch SQL
-- Revision: 14 -> 15

-- Drop indexes for logs table
DROP INDEX IF EXISTS logs_app_idx;
DROP INDEX IF EXISTS logs_app_lt_idx;
DROP INDEX IF EXISTS logs_branch_complex_idx;
DROP INDEX IF EXISTS logs_branch_complex_lt_idx;
DROP INDEX IF EXISTS logs_branch_version_complex_idx;
DROP INDEX IF EXISTS logs_branch_version_complex_lt_idx;
DROP INDEX IF EXISTS logs_component_complex_idx;
DROP INDEX IF EXISTS logs_component_complex_lt_idx;
DROP INDEX IF EXISTS logs_data_gin_idx;
DROP INDEX IF EXISTS logs_id_seq_idx;
DROP INDEX IF EXISTS logs_level_idx;
DROP INDEX IF EXISTS logs_msg_count_idx;
DROP INDEX IF EXISTS logs_timestamp_idx;
DROP INDEX IF EXISTS logs_timestamp_seq_idx;
DROP INDEX IF EXISTS logs_to_tsvector_idx;
DROP INDEX IF EXISTS logs_version_complex_idx;
DROP INDEX IF EXISTS logs_version_complex_lt_idx;
