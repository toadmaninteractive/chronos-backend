-- Patch SQL
-- Revision: 7 -> 8

-- Set created_at column default value and make it non-null
ALTER TABLE crash_reports ALTER COLUMN created_at SET DEFAULT current_timestamp;
ALTER TABLE crash_reports ALTER COLUMN created_at SET NOT NULL;
