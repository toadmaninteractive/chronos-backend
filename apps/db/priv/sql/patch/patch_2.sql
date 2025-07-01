-- Patch SQL
-- Revision: 1 -> 2

-- Add props table
CREATE TABLE props
(
    name varchar PRIMARY KEY CHECK (TRIM(name) <> ''),
    value varchar
) WITHOUT OIDS;

-- Alter logs table and add related indexes
ALTER TABLE logs ADD COLUMN version text NOT NULL DEFAULT 'None';

CREATE INDEX logs_app_idx ON logs (app);
CREATE INDEX logs_component_complex_idx ON logs (app, component);
CREATE INDEX logs_branch_complex_idx ON logs (app, component, branch);
CREATE INDEX logs_version_complex_idx ON logs (app, component, version);
CREATE INDEX logs_timestamp_idx ON logs (timestamp);
CREATE INDEX logs_level_idx ON logs (level);
CREATE INDEX logs_data_gin_idx ON logs USING gin(data);
