-module(db_if_crash_reports).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    apps/0,
    versions/3,
    metadata_keys/4,
    get_one/1,
    get/4,
    get_count/0,
    get_expired/6,
    get_expired_count/2,
    create/10,
    wipe/1,
    wipe_many/1,
    exists/1
]).

%% API

-spec apps() ->
    {'ok', [binary()]} | {'error', Reason :: atom()}.

apps() ->
    Statement = <<
        "SELECT app ",
        "FROM acbv_crash_reports ",
        "GROUP BY app ",
        "ORDER BY app "
    >>,
    case db_query:select(Statement, []) of
        {ok, Items} -> {ok, [App || #{<<"app">> := App} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec versions(App, Component, Branch) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Result :: {'ok', [binary()]} | {'error', Reason :: atom()}.

versions(App, Component, Branch) ->
    Query = <<
        "SELECT version ",
        "FROM acbv_crash_reports ",
        "WHERE app = $1 AND component = $2 AND branch = $3 ",
        "GROUP BY version ",
        "ORDER BY version "
    >>,
    case db_query:select(Query, [App, Component, Branch]) of
        {ok, Items} -> {ok, [Version || #{<<"version">> := Version} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec metadata_keys(App, Component, Branch, Versions) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    Result :: {'ok', [binary()]} | {'error', Reason :: atom()}.

metadata_keys(App, Component, Branch, _Versions = undefined) ->
    Query = <<
        "SELECT DISTINCT jsonb_object_keys(\"data\") AS field ",
        "FROM ( ",
            "SELECT data FROM crash_reports ",
            "WHERE app = $1 AND component = $2 AND branch = $3 ",
            "ORDER BY created_at DESC ",
            "LIMIT 10000 ",
        ") as q ",
        "ORDER BY field"
    >>,
    Params = [App, Component, Branch],
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [Field || #{<<"field">> := Field} <- Items]};
        {error, Reason} -> {error, Reason}
    end;

metadata_keys(App, Component, Branch, Versions) when is_list(Versions) ->
    VsSql = iolist_to_binary(lists:join(", ", [[$', V, $'] || V <- Versions])),
    Query = <<
        "SELECT DISTINCT jsonb_object_keys(\"data\") AS field ",
        "FROM crash_reports ",
        "WHERE app = $1 AND component = $2 AND branch = $3 AND version IN (", VsSql/binary, ") ",
        "ORDER BY field"
    >>,
    Params = [App, Component, Branch],
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [Field || #{<<"field">> := Field} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_one(Id :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(Id) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM crash_reports AS cr ",
        (common_joins())/binary,
        "WHERE cr.id = $1 "
    >>,
    case db_query:select_one(Query, [Id]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(OrderBy, OrderDir, Offset, Limit) -> Result when
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM crash_reports AS cr ",
        (common_joins())/binary,
        Filter/binary
    >>,
    db_query:select(Statement, []).

-spec get_count() ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count() ->
    Statement = <<"SELECT count(*)::bigint AS count FROM crash_reports">>,
    case db_query:select_one(Statement, []) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_expired(App, OrderBy, OrderDir, Offset, Limit, LifetimeDays) -> Result when
    App :: binary(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    LifetimeDays :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_expired(App, OrderBy, OrderDir, Offset, Limit, LifetimeDays) ->
    Interval = <<"'", (integer_to_binary(LifetimeDays))/binary, " days'">>,
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM crash_reports AS cr ",
        (common_joins())/binary,
        "WHERE ",
            "LOWER(TRIM(cr.app)) = $1 ",
            "AND (cr.created_at < NOW() - INTERVAL ", Interval/binary, ") ",
        Filter/binary
    >>,
    db_query:select(Query, [App]).

-spec get_expired_count(App, LifetimeDays) -> Result when
    App :: binary(),
    LifetimeDays :: non_neg_integer(),
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_expired_count(App, LifetimeDays) ->
    Interval = <<"'", (integer_to_binary(LifetimeDays))/binary, " days'">>,
    Query = <<
        "SELECT count(*)::bigint AS count ",
        "FROM crash_reports AS cr ",
        "WHERE ",
            "LOWER(TRIM(cr.app)) = $1 ",
            "AND (cr.created_at < NOW() - INTERVAL ", Interval/binary, ") "
    >>,
    case db_query:select_one(Query, [App]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(App, Component, Branch, Version, GameEngine, Username, Filename, Reason, UserComment, Data) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    GameEngine :: binary() | 'undefined',
    Username :: binary(),
    Filename :: binary(),
    Reason :: binary(),
    UserComment :: binary() | 'undefined',
    Data :: jsx:json_term(),
    Result :: {'ok', BuildId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(App, Component, Branch, Version, GameEngine, Username, Filename, Reason, UserComment, Data) ->
    Query = <<
        "INSERT INTO crash_reports (app, component, branch, version, game_engine, username, filename, reason, user_comment, data) ",
        "VALUES (LOWER(TRIM($1)), LOWER(TRIM($2)), LOWER(TRIM($3)), LOWER(TRIM($4)), LOWER(TRIM($5)), LOWER(TRIM($6)), TRIM($7), TRIM($8), TRIM($9), $10::jsonb) ",
        "RETURNING id::bigint"
    >>,
    Params = [App, Component, Branch, Version, GameEngine, Username, Filename, Reason, UserComment, jsx:encode(Data)],
    case db_query:insert(Query, Params) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := BuildId}] = db_util:result_to_json(Columns, Rows),
            {ok, BuildId};
        {error, Other} ->
            {error, Other}
    end.

-spec wipe(Id :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

wipe(Id) ->
    Query = <<"DELETE FROM crash_reports WHERE id = $1">>,
    case db_query:delete(Query, [Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec wipe_many(Ids) -> Result when
    Ids :: [non_neg_integer()],
    Result :: 'ok' | {'error', Reason :: atom()}.

wipe_many([]) ->
    ok;

wipe_many(Ids) ->
    Query = iolist_to_binary([
        "DELETE FROM crash_reports WHERE id IN (",
            [[case I of 1 -> ""; _ -> ", " end, "$", integer_to_list(I)] || I <- lists:seq(1, length(Ids))],
        ")"
    ]),
    db_query:delete(Query, Ids).

-spec exists(Id :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Id) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM crash_reports WHERE id = $1">>,
    case db_query:select_one(Statement, [Id]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % cr : crash_reports
    <<
        "cr.id AS id, ",
        "cr.app AS app, ",
        "cr.component AS component, ",
        "cr.branch AS branch, ",
        "cr.version AS version, ",
        "cr.game_engine AS game_engine, ",
        "cr.username AS username, ",
        "cr.filename AS filename, ",
        "cr.reason AS reason, ",
        "cr.user_comment AS user_comment, ",
        "cr.data AS data, ",
        "cr.created_at AS created_at "
    >>.

common_joins() ->
    <<>>.
