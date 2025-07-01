-- Patch SQL
-- Revision: 9 -> 10

-- Materialized view to hold unique tuples of {app, component, branch, version} for logs and crash reports
CREATE MATERIALIZED VIEW IF NOT EXISTS apps_mview AS (
    WITH cte AS (
        SELECT
            LOWER(TRIM(app)) AS app,
            LOWER(TRIM(component)) AS component,
            LOWER(TRIM(branch)) AS branch,
            LOWER(TRIM(version)) AS version
        FROM logs
        GROUP BY app, component, branch, version
        UNION
        SELECT
            LOWER(TRIM(app)) AS app,
            LOWER(TRIM(component)) AS component,
            LOWER(TRIM(branch)) AS branch,
            LOWER(TRIM(version)) AS version
        FROM crash_reports
        GROUP BY app, component, branch, version
    )
    SELECT app, component, branch, version
    FROM cte
    GROUP BY app, component, branch, version
);

-- Unique index to refresh the materialized view concurrently
CREATE UNIQUE INDEX apps_mview_u_index ON apps_mview (app, component, branch, version);
