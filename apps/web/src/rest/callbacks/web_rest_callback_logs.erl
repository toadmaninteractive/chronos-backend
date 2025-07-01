-module(web_rest_callback_logs).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_logs/13,
    get_log_versions/3,
    get_log_metadata_keys/4,
    get_timeline/11
]).

-define(query_limit, 1000).

%% API

-spec get_logs(App, Component, Branch, Versions, From, To, Levels, Search, OrderBy, OrderDir, Offset, Limit, Metadata) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    From :: iso8601:datetimems(),
    To :: iso8601:datetimems(),
    Levels :: [web_protocol:log_level()],
    Search :: binary(),
    OrderBy :: web_protocol:log_entry_order_by(),
    OrderDir :: data_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Metadata :: proplists:proplist(),
    Result :: [web_protocol:log_entry()].

get_logs(App, Component, Branch, Versions, From, To, Levels, Search, OrderBy, OrderDir, Offset, Limit, Metadata) ->
    Query = db_query_builder:new("logs", ["id", "app", "component", "branch", "version", "level", "timestamp", "data", "message", "msg_count", "seq_id"]),
    Query0 = db_query_builder:where_map(Query, #{
        "app" => App,
        "component" => Component,
        "branch" => Branch
    }),
    % Query1 = db_query_builder:where_opt(Query0, Version, ["version = ", db_query_builder:param(Version)]),
    Vs = web_util:trim_list(Versions, fun(E) -> util_binary:trim(web_util:sanitize_sql(E)) end),
    Query1 = case Vs of
        _ when is_list(Vs), Vs =/= [] -> db_query_builder:where(Query0, ["version IN (", lists:join(",", [[$', V, $'] || V <- Vs]), ")"]);
        _ -> Query0
    end,
    Query2 = db_query_builder:where_opt(Query1, From, ["timestamp >= ", db_query_builder:param(From)]),
    Query3 = db_query_builder:where_opt(Query2, To, ["timestamp <= ", db_query_builder:param(To)]),
    Query4 = db_query_builder:where_opt(Query3, Search, [
        "(message ILIKE '%' || ", db_query_builder:param(Search), " || '%' OR data::text ILIKE '%' || ", db_query_builder:param(Search), " || '%')"
    ]),
    OrderByStr = ?yesno(OrderBy =:= undefined, "id", atom_to_list(OrderBy)),
    OrderDirStr = ?yesno(OrderDir =:= undefined, "desc", atom_to_list(OrderDir)),
    SeqOrderDirStr = case OrderByStr of
        "id" -> OrderDirStr;
        "timestamp" -> OrderDirStr;
        _ -> "asc"
    end,
    Query5 = db_query_builder:order_by(Query4, OrderByStr ++ " " ++ OrderDirStr ++ ", seq_id " ++ SeqOrderDirStr),
    Query6 = db_query_builder:offset(Query5, Offset),
    Query7 = db_query_builder:limit(Query6, ?yesno(is_integer(Limit), min(Limit, ?query_limit), ?query_limit)),
    Query8 = case Levels of
        _ when is_list(Levels), Levels =/= [] ->
            db_query_builder:where(Query7, ["level IN (", lists:join(",", [[$', data_protocol:log_level_to_json(L), $'] || L <- Levels]), ")"]);
        _ -> Query7
    end,
    Query9 = case is_list(Metadata) of
        true ->
            lists:foldl(fun({K, V}, AccIn) ->
                V1 = case V of
                    _ when is_binary(V) -> ?yesno(jsx:is_json(V), V, <<"\"", (web_util:enquote_sql(V))/binary, "\"">>);
                    _ -> V
                end,
                db_query_builder:where_opt(AccIn, V1, ["data->'", K, "' = ", db_query_builder:param(V1), "::jsonb"])
            end, Query8, [{K, V} || {K, V} <- Metadata, is_valid_field(K)]);
        false -> Query8
    end,
    {Sql, Params} = db_query_builder:compile(Query9),
    logger:debug("SQL: ~s~nArgs: ~p~n", [Sql, Params], #{caption => ?MODULE}),
    {ok, Result} = db_query:select(Sql, Params),
    [web_protocol:log_entry_from_json(Log) || Log <- Result].

-spec get_log_versions(App, Component, Branch) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Result :: [binary()].

get_log_versions(App, Component, Branch) ->
    {ok, Versions} = db_if_logs:versions(App, Component, Branch),
    Versions.

-spec get_log_metadata_keys(App, Component, Branch, Versions) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    Result :: [binary()].

get_log_metadata_keys(App, Component, Branch, Versions) ->
    Vs = web_util:trim_list(Versions, fun(E) -> util_binary:trim(web_util:sanitize_sql(E)) end),
    Vs1 = ?yesno(Vs =:= [], undefined, Vs),
    {ok, MetadataKeys} = db_if_logs:metadata_keys(App, Component, Branch, Vs1),
    MetadataKeys.

-spec get_timeline(App, Component, Branch, Versions, DateFrom, DateTo, BinUnit, BinDuration, Levels, Search, Metadata) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    DateFrom :: iso8601:datetimems(),
    DateTo :: iso8601:datetimems(),
    BinUnit :: data_protocol:bin_unit(),
    BinDuration :: non_neg_integer(),
    Levels :: [web_protocol:log_level()] | 'undefined',
    Search :: binary() | 'undefined',
    Metadata :: proplists:proplist() | 'undefined',
    Result :: [web_protocol:timeline_entry()].

get_timeline(App, Component, Branch, Versions, DateFrom, DateTo, BinUnit, BinDuration, Levels, Search, Metadata) ->
    % Estimate time bin
    TimeBin = lists:flatten([integer_to_list(BinDuration), " ", bin_unit_to_string(BinUnit)]),

    Query = db_query_builder:new("logs", [
        "level",
        "date_bin('", TimeBin, "', timestamp, '2000-01-01')::timestamp AS time_bin",
        "(CASE level WHEN 'trace' THEN sum(msg_count) ELSE 0 END)::int AS num_trace",
        "(CASE level WHEN 'debug' THEN sum(msg_count) ELSE 0 END)::int AS num_debug",
        "(CASE level WHEN 'info' THEN sum(msg_count) ELSE 0 END)::int AS num_info",
        "(CASE level WHEN 'notice' THEN sum(msg_count) ELSE 0 END)::int AS num_notice",
        "(CASE level WHEN 'warning' THEN sum(msg_count) ELSE 0 END)::int AS num_warning",
        "(CASE level WHEN 'error' THEN sum(msg_count) ELSE 0 END)::int AS num_error",
        "(CASE level WHEN 'fatal' THEN sum(msg_count) ELSE 0 END)::int AS num_fatal"
    ]),
    Query0 = db_query_builder:where_map(Query, #{"app" => App, "component" => Component, "branch" => Branch}),
    Query1 = db_query_builder:where(Query0, ["timestamp >= ", db_query_builder:param(DateFrom)]),
    Query2 = db_query_builder:where(Query1, ["timestamp <= ", db_query_builder:param(DateTo)]),
    % Query3 = db_query_builder:where_opt(Query2, Version, ["version = ", db_query_builder:param(Version)]),
    Vs = web_util:trim_list(Versions, fun(E) -> util_binary:trim(web_util:sanitize_sql(E)) end),
    Query3 = case Vs of
        _ when is_list(Vs), Vs =/= [] -> db_query_builder:where(Query2, ["version IN (", lists:join(",", [[$', V, $'] || V <- Vs]), ")"]);
        _ -> Query2
    end,
    Query4 = case Levels of
        undefined -> Query3;
        _ -> db_query_builder:where(Query3, ["level IN (", lists:join(",", [[$', data_protocol:log_level_to_json(L), $'] || L <- Levels]), ")"])
    end,
    Query5 = db_query_builder:where_opt(Query4, Search, [
        "(message ILIKE '%' || ", db_query_builder:param(Search), " || '%' OR data::text ILIKE '%' || ", db_query_builder:param(Search), " || '%')"
    ]),
    Query6 = case is_list(Metadata) of
        true ->
            lists:foldl(fun({K, V}, AccIn) ->
                V1 = case V of
                    _ when is_binary(V) -> ?yesno(jsx:is_json(V), V, <<"\"", (web_util:enquote_sql(V))/binary, "\"">>);
                    _ -> V
                end,
                db_query_builder:where_opt(AccIn, V1, ["data->'", K, "' = ", db_query_builder:param(V1), "::jsonb"])
            end, Query5, [{K, V} || {K, V} <- Metadata, is_valid_field(K)]);
        false -> Query5
    end,
    Query7 = db_query_builder:group_by(Query6, "level, time_bin"),
    Query8 = db_query_builder:order_by(Query7, "time_bin"),
    {InnerSql, Params} = db_query_builder:compile(Query8),
    Sql = <<
        "SELECT ",
            "time_bin, ",
            "sum(num_trace) AS num_trace, ",
            "sum(num_debug) AS num_debug, ",
            "sum(num_info) AS num_info, ",
            "sum(num_notice) AS num_notice, ",
            "sum(num_warning) AS num_warning, ",
            "sum(num_error) AS num_error, ",
            "sum(num_fatal) AS num_fatal ",
        "FROM (", InnerSql/binary, ") AS t ",
        "GROUP BY time_bin ",
        "ORDER BY time_bin ",
        "LIMIT 100000"
    >>,
    logger:debug("SQL: ~s~nArgs: ~p~n", [Sql, Params], #{caption => ?MODULE}),
    {ok, Result} = db_query:select(Sql, Params),
    [web_protocol:timeline_entry_from_json(Json) || Json <- Result].

%% Local functions

is_valid_field(Field) ->
    re:run(Field, <<"^[A-Za-z0-9-_]+$">>, [global]) =/= nomatch.

bin_unit_to_string(msec) -> "milliseconds";
bin_unit_to_string(sec) -> "seconds";
bin_unit_to_string(min) -> "minutes";
bin_unit_to_string(hour) -> "hours";
bin_unit_to_string(day) -> "days".
