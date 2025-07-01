-module(db_if_logs).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    last_id/0,
    apps/0,
    versions/3,
    metadata_keys/4,
    get_after/1,
    create/8,
    create_many/1,
    create_many/5,
    wipe/2
]).

%% API

-spec last_id() ->
    {'ok', LastId :: non_neg_integer()} | {'error', Reason :: atom()}.

last_id() ->
    Statement = <<"SELECT MAX(id)::bigint AS last_id FROM logs">>,
    case db_query:select_one(Statement, []) of
        {ok, #{<<"last_id">> := LastId}} -> {ok, LastId};
        {error, Reason} -> {error, Reason}
    end.

-spec apps() ->
    {'ok', [binary()]} | {'error', Reason :: atom()}.

apps() ->
    Statement = <<
        "SELECT app ",
        "FROM acbv_logs ",
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
        "FROM acbv_logs ",
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
            "SELECT data FROM logs ",
            "WHERE app = $1 AND component = $2 AND branch = $3 ",
            "ORDER BY timestamp DESC ",
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
        "FROM logs ",
        "WHERE app = $1 AND component = $2 AND branch = $3 AND version IN (", VsSql/binary, ") ",
        "ORDER BY field"
    >>,
    Params = [App, Component, Branch],
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [Field || #{<<"field">> := Field} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_after(Id) -> Result when
    Id :: non_neg_integer() | 'null',
    Result :: {'ok', Items :: [web_protocol:log_entry()]} | {'error', Reason :: atom()}.

get_after(Id) ->
    WherePart = ?yesno(is_integer(Id), <<" WHERE id > $1 ">>, <<>>),
    Params = ?yesno(is_integer(Id), [Id], []),
    Query = <<
        "SELECT ",
            "id, ",
            "TRIM(LOWER(app)) AS app, ",
            "TRIM(LOWER(component)) AS component, ",
            "TRIM(LOWER(branch)) AS branch, ",
            "version, ",
            "level, ",
            "timestamp, ",
            "data, ",
            "message, ",
            "msg_count, ",
            "seq_id ",
        "FROM logs ", WherePart/binary
    >>,
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [web_protocol:log_entry_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(App, Component, Branch, Version, Timestamp, Level, Data, Message) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Timestamp :: calendar:datetime(),
    Level :: web_protocol:log_level(),
    Data :: jsx:json_term(),
    Message :: binary(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

create(App, Component, Branch, Version, Timestamp, Level, Data, Message) ->
    Query = <<
        "INSERT INTO logs (app, component, branch, version, timestamp, level, data, message) ",
        "VALUES (LOWER(TRIM($1)), LOWER(TRIM($2)), LOWER(TRIM($3)), LOWER(TRIM($4)), $5, $6, $7, $8) ",
        "RETURNING id::bigint"
    >>,
    Params = [App, Component, Branch, Version, Timestamp, Level, jsx:encode(Data), Message],
    case db_query:insert(Query, Params) of
        {ok, _, Cols, Rows} ->
            [#{<<"id">> := Id}|_] = db_util:result_to_json(Cols, Rows),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_many(App, Component, Branch, Version, Logs) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Logs :: [Log],
    Log :: {Timestamp, Level, Data, Message, MsgCount, SeqId},
    Timestamp :: calendar:datetime(),
    Level :: web_protocol:log_level(),
    Data :: jsx:json_term(),
    Message :: binary(),
    MsgCount :: non_neg_integer(),
    SeqId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

create_many(App, Component, Branch, Version, Logs) ->
    create_many([
        {App, Component, Branch, Version,
            Timestamp, Level, Data, Message, MsgCount, SeqId}
        || {Timestamp, Level, Data, Message, MsgCount, SeqId} <- Logs
    ]).

-spec create_many(Logs) -> Result when
    Logs :: [Log],
    Log :: {App, Component, Branch, Version,
            Timestamp, Level, Data, Message, MsgCount, SeqId},
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Timestamp :: calendar:datetime(),
    Level :: web_protocol:log_level(),
    Data :: jsx:json_term(),
    Message :: binary(),
    MsgCount :: non_neg_integer(),
    SeqId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

create_many(Logs) ->
    Sql = iolist_to_binary([
        "INSERT INTO logs (app, component, branch, version, timestamp, level, data, message, msg_count, seq_id) VALUES ",
        [begin
            P = 10 * I,
            io_lib:format(
                "~s (LOWER(TRIM($~b)), LOWER(TRIM($~b)), LOWER(TRIM($~b)), LOWER(TRIM($~b)), $~b, $~b, $~b::jsonb, TRIM($~b), $~b, $~b)",
                [
                    case I of 0 -> " "; _ -> "," end,
                    P + 1, P + 2, P + 3, P + 4, P + 5, P + 6, P + 7, P + 8, P + 9, P + 10
                ]
            )
        end || {I, _} <- lists:zip(lists:seq(0, length(Logs) - 1), Logs)]
    ]),
    Params = lists:flatten([
        [
            App, Component, Branch, Version,
            TimeStamp, Level, jsx:encode(Data), Message,
            MsgCount, SeqId
        ] || {
            App, Component, Branch, Version,
            TimeStamp, Level, Data, Message,
            MsgCount, SeqId
        } <- Logs
    ]),
    db_query:insert(Sql, Params).

-spec wipe(App, LogLifetimeDays) -> Result when
    App :: binary(),
    LogLifetimeDays :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

wipe(App, LogLifetimeDays) ->
    Interval = <<"'", (integer_to_binary(LogLifetimeDays))/binary, " days'">>,
    Query = <<"DELETE FROM logs WHERE LOWER(TRIM(app)) = LOWER(TRIM($1)) AND (timestamp < NOW() - INTERVAL ", Interval/binary, ")">>,
    case db_query:delete(Query, [App]) of
        {ok, NumDeleted} -> {ok, NumDeleted};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
