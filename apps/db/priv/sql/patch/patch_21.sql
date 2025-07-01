-- Patch SQL
-- Revision: 20 -> 21

-- Convert crash_reports table varchar columns to text
ALTER TABLE crash_reports ALTER COLUMN app TYPE text;
ALTER TABLE crash_reports ALTER COLUMN component TYPE text;
ALTER TABLE crash_reports ALTER COLUMN branch TYPE text;
ALTER TABLE crash_reports ALTER COLUMN version TYPE text;
ALTER TABLE crash_reports ALTER COLUMN game_engine TYPE text;
ALTER TABLE crash_reports ALTER COLUMN username TYPE text;
ALTER TABLE crash_reports ALTER COLUMN filename TYPE text;
ALTER TABLE crash_reports ALTER COLUMN reason TYPE text;
