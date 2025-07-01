-- Patch SQL
-- Revision: 5 -> 6

-- Settings
CREATE TABLE settings
(
    param varchar PRIMARY KEY CHECK (TRIM(param) <> ''),
    type varchar NOT NULL CHECK (TRIM(type) <> ''),
    value text DEFAULT NULL
) WITHOUT OIDS;

-- Add new settings
INSERT INTO settings ("param", "type", "value") VALUES 
    ('api_key', 'string', NULL);
