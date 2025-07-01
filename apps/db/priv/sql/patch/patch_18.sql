-- Patch SQL
-- Revision: 17 -> 18

-- Add acbv_quad generated column to logs table
ALTER TABLE logs ADD COLUMN IF NOT EXISTS acbv_quad text GENERATED ALWAYS AS (app || '\t' || component || '\t' || branch || '\t' || version) STORED;

-- Re-create indexes for logs table
CREATE INDEX logs_acbv_quad_idx ON logs USING BTREE(acbv_quad);
CREATE INDEX logs_acb_tseq_idx ON logs USING BTREE(app, component, branch, "timestamp" DESC, seq_id DESC);
CREATE INDEX logs_acbv_tseq_idx ON logs USING BTREE(app, component, branch, version, "timestamp" DESC, seq_id DESC);
CREATE INDEX logs_acbvm_tseq_trgm_gin_idx ON logs USING GIN (app, component, branch, version, "timestamp", seq_id, message gin_trgm_ops);
