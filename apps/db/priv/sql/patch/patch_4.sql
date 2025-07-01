-- Patch SQL
-- Revision: 3 -> 4

-- Add msg_count column and related index to logs table
ALTER TABLE logs ADD COLUMN msg_count integer NOT NULL DEFAULT 1;
CREATE INDEX logs_msg_count_idx ON logs (msg_count);
