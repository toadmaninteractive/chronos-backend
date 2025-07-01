-module(db_setup).

%% This module uses direct connections instead of poolboy pools

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("db.hrl").

%% Exported functions

-export([
    db_version/0,
    setup/0,
    migrate/0
]).

%% API

-spec db_version() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: atom()}.

db_version() ->
    connect_and_execute(fun db_version/1).

-spec setup() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: atom()}.

setup() ->
    connect_and_execute(fun setup/1).

-spec migrate() ->
    {'latest', Version :: non_neg_integer()}
    | {'updated', FromVersion :: non_neg_integer(), ToVersion :: non_neg_integer()}
    | {'error', Reason :: atom()}.

migrate() ->
    connect_and_execute(fun(Connection) ->
        {ok, CurrentVersion} = db_version(Connection),
        {ok, LastVersion} = db_config:last_version(),
        migrate(Connection, CurrentVersion, LastVersion)
    end).

%% Local functions

spawn_worker() ->
    epgsql:connect(db_config:hostname(), db_config:username(), db_config:password(), #{
        database => db_config:database(),
        timeout => db_config:connect_timeout()
    }).

connect_and_execute(Fun) when is_function(Fun, 1) ->
    case spawn_worker() of
        {ok, Connection} ->
            Result = try Fun(Connection) catch _:_ -> {error, exception} end,
            epgsql:close(Connection),
            Result;
        {error, Reason} ->
            {error, Reason}
    end;
connect_and_execute(_) ->
    {error, badarg}.

%% Safe queries

safe_query(Connection, Fun, Args) ->
    try
        erlang:apply(Fun, [Connection | Args])
    catch
        _:{noproc, _} -> {error, disconnected};
        _:_Exception -> {error, exception}
    end.

safe_squery(Connection, Sql) ->
    safe_query(Connection, fun epgsql:squery/2, [Sql]).

safe_equery(Connection, Sql, Params) ->
    safe_query(Connection, fun epgsql:equery/3, [Sql, Params]).

safe_transaction(Connection, Sql) ->
    safe_query(Connection, fun epgsql:with_transaction/2, [fun(C) ->
        Statements = [util_binary:trim(S) || S <- binary:split(Sql, <<";">>, [global])],
        Result = safe_squery(C, Sql),
        ResultList = ?yesno(is_list(Result), Result, [Result]),
        ErrorCount = lists:foldl(fun({Index, QR}, Acc) ->
            LQR = tuple_to_list(QR),
            case LQR of
                [error] ->
                    logger:error("Statement #~B failed", [Index], #{caption => ?MODULE});
                [error, Error = #error{}] ->
                    logger:error("Statement #~B failed with reason \"~s\" (~s): ~s~n~s", [
                        Index,
                        Error#error.codename,
                        Error#error.code,
                        Error#error.message,
                        lists:nth(Index, Statements)
                    ], #{caption => ?MODULE});
                [error, Error] ->
                    logger:error("Statement #~B failed with reason \"~p\"", [Index, Error], #{caption => ?MODULE});
                [ok|_] -> ok
            end,
            ?yesno(hd(LQR) =/= error, Acc, Acc + 1)
        end, 0, lists:zip(lists:seq(1, length(ResultList)), ResultList)),
        ?yesno(ErrorCount =:= 0, ok, failure)
    end]).

%% Database version getter and setter

db_version(Connection) ->
    Query = <<"SELECT value::integer FROM props WHERE name = lower($1)">>,
    Params = [<<"db_version">>],
    case safe_equery(Connection, Query, Params) of
        {ok, Columns, Values} ->
            case db_util:result_to_json(Columns, Values) of
                [] -> {ok, 0};
                [#{<<"value">> := DbVersion}|_] -> {ok, DbVersion}
            end;
        {error, #error{codename = undefined_table}} ->
            {ok, 0};
        {error, Reason} ->
            {error, Reason}
    end.

set_db_version(Connection, Value) ->
    Query = <<
        "INSERT INTO props VALUES (lower($1), $2) ",
        "ON CONFLICT (name) DO UPDATE SET value = $2"
    >>,
    Params = [<<"db_version">>, Value],
    case safe_equery(Connection, Query, Params) of
        {ok, 1} -> ok;
        _ -> error
    end.

%% Initial setup

setup(Connection) ->
    {ok, LastVersion} = db_config:last_version(),
    try
        {ok, Sql} = db_config:setup_sql(),
        ok = safe_transaction(Connection, Sql),
        ok = set_db_version(Connection, LastVersion),
        {ok, LastVersion}
    catch
        Class:Reason:StackTrace ->
            logger:error("Setup version ~p FAILED", [LastVersion], #{caption => ?MODULE}),
            logger:error("Reason: ~p:~p", [Class, Reason], #{caption => ?MODULE, stacktrace => StackTrace}),
            {error, Reason}
    end.

%% Migration

find_migration(Migrations, V, ToV) ->
    case lists:keyfind(ToV, 1, Migrations) of
        false -> throw({no_migration, {V, ToV}});
        {_, Callback} -> Callback
    end.

migrate(_Connection, FromVersion, ToVersion) when ToVersion == FromVersion ->
    {latest, ToVersion};
migrate(_Connection, FromVersion, ToVersion) when ToVersion < FromVersion ->
    {error, current_version_is_higher};
migrate(Connection, FromVersion, ToVersion) ->
    try
        {ok, Migrations} = db_config:migrations(),
        migrate(Connection, Migrations, FromVersion, ToVersion),
        {updated, FromVersion, ToVersion}
    catch
        Class:Reason:StackTrace ->
            logger:error("Migration ~p to ~p FAILED", [FromVersion, ToVersion], #{caption => ?MODULE}),
            logger:error("Reason: ~p:~p", [Class, Reason], #{caption => ?MODULE, stacktrace => StackTrace}),
            {error, Reason}
    end.

migrate(_Connection, [], V, ToV) when V =/= ToV ->
    throw(no_migrations);
migrate(_Connection, _Migrations, V, ToV) when V >= ToV ->
    ok;
migrate(Connection, Migrations, V, ToV) ->
    NextV = V + 1,
    logger:info("Migrating from ~p to ~p...", [V, NextV], #{caption => ?MODULE}),
    case find_migration(Migrations, V, NextV) of
        {sql, Filename} ->
            {ok, Sql} = db_config:patch_sql(Filename),
            ok = safe_transaction(Connection, Sql);
        Module when is_atom(Module) ->
            erlang:apply(Module, start, [Connection])
    end,
    ok = set_db_version(Connection, NextV),
    logger:info("Migration from ~p to ~p COMPLETED", [V, NextV], #{caption => ?MODULE}),
    migrate(Connection, Migrations, NextV, ToV).
