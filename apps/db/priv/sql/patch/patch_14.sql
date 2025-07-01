-- Patch SQL
-- Revision: 13 -> 14

-- Add new settings
INSERT INTO settings ("param", "type", "value") VALUES 
    ('personnel_session_duration', 'int', '31536000');
