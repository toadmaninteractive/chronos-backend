-- Patch SQL
-- Revision: 2 -> 3

-- Add new indexes to logs table
CREATE INDEX logs_app_lt_idx ON logs (LOWER(TRIM(app)));
CREATE INDEX logs_component_complex_lt_idx ON logs (LOWER(TRIM(app)), LOWER(TRIM(component)));
CREATE INDEX logs_branch_complex_lt_idx ON logs (LOWER(TRIM(app)), LOWER(TRIM(component)), LOWER(TRIM(branch)));
CREATE INDEX logs_version_complex_lt_idx ON logs (LOWER(TRIM(app)), LOWER(TRIM(component)), LOWER(TRIM(version)));
CREATE INDEX logs_branch_version_complex_idx ON logs (app, component, branch, version);
CREATE INDEX logs_branch_version_complex_lt_idx ON logs (LOWER(TRIM(app)), LOWER(TRIM(component)), LOWER(TRIM(branch)), LOWER(TRIM(version)));