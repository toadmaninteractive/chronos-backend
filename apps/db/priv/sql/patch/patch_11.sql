-- Patch SQL
-- Revision: 10 -> 11

-- Materialized view to hold unique metadata key set for logs
CREATE MATERIALIZED VIEW IF NOT EXISTS apps_metadata_mview AS (
    SELECT
        amv.app,
        amv.component,
        amv.branch,
        amv.version,
        (
            SELECT jsonb_agg(q.keys) FROM (
                SELECT DISTINCT jsonb_object_keys("data") AS keys
                FROM logs AS l
                WHERE
                    LOWER(TRIM(l.app)) = amv.app
                    AND LOWER(TRIM(l.component)) = amv.component
                    AND LOWER(TRIM(l.branch)) = amv.branch
                    AND LOWER(TRIM(l.version)) = amv.version
            ) AS q
        ) AS metadata_keys
    FROM apps_mview AS amv
);

-- Unique index to refresh the materialized view concurrently
CREATE UNIQUE INDEX apps_metadata_mview_u_index ON apps_metadata_mview (app, component, branch, version);
