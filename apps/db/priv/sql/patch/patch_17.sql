-- Patch SQL
-- Revision: 16 -> 17

-- Drop materialized views and related indexes
DROP INDEX IF EXISTS apps_mview_u_index;
DROP MATERIALIZED VIEW IF EXISTS apps_mview CASCADE;

DROP INDEX IF EXISTS apps_metadata_mview_u_index;
DROP MATERIALIZED VIEW IF EXISTS apps_metadata_mview CASCADE;
