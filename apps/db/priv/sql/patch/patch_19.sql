-- Patch SQL
-- Revision: 18 -> 19

-- Add acbv_logs view
CREATE OR REPLACE VIEW acbv_logs AS
    SELECT acbv[1] AS app, acbv[2] AS component, acbv[3] AS branch, acbv[4] AS version
    FROM (
        WITH RECURSIVE t AS (
            (SELECT acbv_quad FROM logs ORDER BY acbv_quad LIMIT 1)
            UNION ALL
            SELECT (SELECT acbv_quad FROM logs WHERE acbv_quad > t.acbv_quad ORDER BY acbv_quad LIMIT 1) FROM t
            WHERE t.acbv_quad IS NOT NULL
        )
        SELECT string_to_array(acbv_quad, '\t') AS acbv FROM t WHERE acbv_quad IS NOT NULL
    ) AS t1;
