-- Patch SQL
-- Revision: 11 -> 12

-- Remove deprecated index
DROP INDEX IF EXISTS logs_seq_id_idx;

-- Add indexes to speed up ordering
CREATE INDEX logs_id_seq_idx ON logs (id, seq_id);
CREATE INDEX logs_timestamp_seq_idx ON logs (timestamp, seq_id);