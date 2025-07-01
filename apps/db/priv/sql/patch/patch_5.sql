-- Patch SQL
-- Revision: 4 -> 5

-- Change id column type to bigint
ALTER TABLE logs ALTER COLUMN id SET DATA TYPE bigint;

-- Add seq_id column and related index to logs table
ALTER TABLE logs ADD COLUMN seq_id integer NOT NULL DEFAULT 1;
CREATE INDEX logs_seq_id_idx ON logs (seq_id);
