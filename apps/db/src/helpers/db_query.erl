-module(db_query).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("db.hrl").

%% Exported functions

-export([
    squery/1,
    squery/2,
    equery/1,
    equery/2,
    equery/3,
    select/2,
    select/3,
    select_one/2,
    select_one/3,
    insert/2,
    insert/3,
    update/2,
    update/3,
    delete/2,
    delete/3,
    execute/2,
    execute/3,
    transaction/1
]).

-export_type([
    statement/0,
    query_opt/0,
    query_opts/0,
    query_params/0,
    query_error/0,
    query_result/0,
    query_single_result/0
]).

-type statement() :: binary() | string().
-type query_opt() :: 'raw_error'.
-type query_opts() :: [query_opt()].
-type query_params() :: [any()].
-type query_error() :: {'error', atom() | epgsql:query_error()}.
-type query_result() :: {'ok', jsx:json_term()}.
-type query_single_result() :: query_result() | 'undefined'.

%% API

-spec squery(Statement :: statement()) ->
    epgsql_cmd_squery:response().

squery(Statement) ->
    squery(?db_pool, Statement).

-spec squery(PoolName :: atom(), Statement :: statement()) ->
    epgsql_cmd_squery:response().

squery(PoolName, Statement) ->
    Result = poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Statement}, db_config:query_timeout())
    end, db_config:connect_timeout()),
    log_error(Result),
    Result.

-spec equery(Statement :: statement()) ->
    epgsql_cmd_squery:response().

equery(Statement) ->
    equery(?db_pool, Statement, []).

-spec equery(Statement :: statement(), Params :: query_params()) ->
    epgsql_cmd_squery:response().

equery(Statement, Params) ->
    equery(?db_pool, Statement, Params).

-spec equery(PoolName :: atom(), Statement :: statement(), Params :: query_params()) ->
    epgsql_cmd_squery:response().

equery(PoolName, Statement, Params) ->
    Result = poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Statement, Params}, db_config:query_timeout())
    end, db_config:connect_timeout()),
    log_error(Result),
    Result.

-spec select(Statement :: statement(), Params :: query_params()) ->
    query_result() | query_error().

select(Statement, Params) ->
    select(Statement, Params, []).

-spec select(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    query_result() | query_error().

select(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case equery(Statement, Params) of
        {ok, Columns, Values} -> {ok, db_util:result_to_json(Columns, Values)};
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec select_one(Statement :: statement(), Params :: query_params()) ->
    query_single_result() | query_error().

select_one(Statement, Params) ->
    select_one(Statement, Params, []).

-spec select_one(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    query_single_result() | query_error().

select_one(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case select(Statement, Params) of
        {ok, [Item|_]} -> {ok, Item};
        {ok, []} -> undefined;
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec insert(Statement :: statement(), Params :: query_params()) ->
    epgsql:ok_reply() | query_error().

insert(Statement, Params) ->
    insert(Statement, Params, []).

-spec insert(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    epgsql:ok_reply() | query_error().

insert(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case equery(Statement, Params) of
        {ok, Rows} -> {ok, Rows};
        {ok, Count, Columns, Rows} -> {ok, Count, Columns, Rows};
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec update(Statement :: statement(), Params :: query_params()) ->
    epgsql:ok_reply() | query_error().

update(Statement, Params) ->
    update(Statement, Params, []).

-spec update(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    epgsql:ok_reply() | query_error().

update(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case equery(Statement, Params) of
        {ok, Count} -> {ok, Count};
        {ok, Count, Columns, Rows} -> {ok, Count, Columns, Rows};
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec delete(Statement :: statement(), Params :: query_params()) ->
    epgsql:ok_reply() | query_error().

delete(Statement, Params) ->
    delete(Statement, Params, []).

-spec delete(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    epgsql:ok_reply() | query_error().

delete(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case equery(Statement, Params) of
        {ok, Count} -> {ok, Count};
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec execute(Statement :: statement(), Params :: query_params()) ->
    'ok' | query_error().

execute(Statement, Params) ->
    execute(Statement, Params, []).

-spec execute(Statement :: statement(), Params :: query_params(), Options :: query_opts()) ->
    'ok' | query_error().

execute(Statement, Params, Options) ->
    RawErrorOpt = lists:member(raw_error, Options),
    case equery(Statement, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Error} -> {error, ?yesno(RawErrorOpt, Error, Error#error.codename)}
    end.

-spec transaction(Arg :: fun() | binary() | string()) ->
    {'ok', #{'result' => jsx:json_term()}} | query_error().

transaction(Arg) ->
    Reply = poolboy:transaction(?db_pool, fun(Worker) ->
        gen_server:call(Worker, {transaction, Arg}, db_config:query_timeout())
    end, db_config:connect_timeout()),
    case Reply of
        {ok, Columns, Values} when is_list(Columns), is_list(Values) ->
            % SELECT
            {ok, #{result => db_util:result_to_json(Columns, Values)}};
        {ok, Count} when is_integer(Count) ->
            % UPDATE / INSERT / DELETE
            {ok, #{count => Count}};
        {ok, Count, Columns, Values} when is_integer(Count), is_list(Columns), is_list(Values) ->
            % UPDATE / INSERT / DELETE + RETURNING
            {ok, #{count => Count, result => db_util:result_to_json(Columns, Values)}};
        {rollback, _Reason} = Error ->
            log_error(Error),
            {error, transaction_rollback};
        {error, #error{codename = Codename}} = Error ->
            log_error(Error),
            {error, Codename}
    end.

%% Local functions

log_error({error, #error{code = Code, codename = Codename, message = Message, severity = Severity} = _Error}) ->
    logger:error("epgsql ~p: ~s ~p ~s", [Severity, Code, Codename, Message]);
log_error({rollback, Reason}) ->
    logger:error("epgsql ROLLBACK ~p", [Reason]);
log_error(_) ->
    ignore.
