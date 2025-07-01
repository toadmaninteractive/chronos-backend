-module(web_rest_callback_errors).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_errors/9
]).

-define(query_limit, 1000).

%% API

-spec get_errors(App, Component, Branch, Versions, From, To, Search, Offset, Limit) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    From :: iso8601:datetimems(),
    To :: iso8601:datetimems(),
    Search :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: [web_protocol:error_entry()].

get_errors(App, Component, Branch, Versions, From, To, Search, Offset, Limit) ->
    Query = db_query_builder:new("logs", ["SUM(msg_count)::integer AS count", "message"]),
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
    Query5 = db_query_builder:where(Query4, ["level IN ('error', 'fatal')"]),
    Query6 = db_query_builder:group_by(Query5, "message"),
    Query7 = db_query_builder:order_by(Query6, "count DESC"),
    Query8 = db_query_builder:offset(Query7, Offset),
    Query9 = db_query_builder:limit(Query8, ?yesno(is_integer(Limit), min(Limit, ?query_limit), ?query_limit)),
    {Sql, Params} = db_query_builder:compile(Query9),
    logger:debug("SQL: ~s~nArgs: ~p~n", [Sql, Params], #{caption => ?MODULE}),
    {ok, Result} = db_query:select(Sql, Params),
    translate_errors(Result).

%% Local functions

translate_errors(Errors) ->
    [translate_error(Error) || Error <- Errors].

translate_error(#{<<"count">> := Count, <<"message">> := Message}) ->
    #error_entry{count = Count, message = Message}.
