-module(web_rest_callback_crash_reports).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_crash_reports/14,
    get_crash_report_versions/3,
    get_crash_report_metadata_keys/4,
    get_crash_report_pdb/4
]).

-define(query_limit, 1000).

%% API

-spec get_crash_reports(App, Component, Branch, Versions, DateFrom, DateTo, Search, GameEngine, Username, Reason, OrderBy, OrderDir, Offset, Limit) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    DateFrom :: iso8601:datetimems(),
    DateTo :: iso8601:datetimems(),
    Search :: binary(),
    GameEngine :: binary(),
    Username :: binary(),
    Reason :: binary() | 'undefined',
    OrderBy :: web_protocol:crash_report_order_by(),
    OrderDir :: data_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: [web_protocol:crash_report()].

get_crash_reports(App, Component, Branch, Versions, DateFrom, DateTo, Search, GameEngine, Username, Reason, OrderBy, OrderDir, Offset, Limit) ->
    Query = db_query_builder:new("crash_reports", ["id", "app", "component", "branch", "version", "game_engine", "username", "filename", "reason", "user_comment", "data", "created_at"]),
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
    Query2 = db_query_builder:where_opt(Query1, DateFrom, ["created_at >= ", db_query_builder:param(DateFrom)]),
    Query3 = db_query_builder:where_opt(Query2, DateTo, ["created_at <= ", db_query_builder:param(DateTo)]),
    Query4 = db_query_builder:where_opt(Query3, Search, [
        "(",
            "reason ILIKE '%' || ", db_query_builder:param(Search), " || '%' ",
            "OR user_comment ILIKE '%' || ", db_query_builder:param(Search), " || '%' ",
            "OR data::text ILIKE '%' || ", db_query_builder:param(Search), " || '%' "
        ")"
    ]),
    Query5 = db_query_builder:where_opt(Query4, GameEngine, ["game_engine = ", db_query_builder:param(GameEngine)]),
    Query6 = db_query_builder:where_opt(Query5, Username, ["username = ", db_query_builder:param(Username)]),
    Query7 = db_query_builder:where_opt(Query6, Reason, ["reason = ", db_query_builder:param(Reason)]),
    OrderByStr = ?yesno(OrderBy =:= undefined, <<"created_at">>, web_protocol:crash_report_order_by_to_json(OrderBy)),
    OrderDirStr = ?yesno(OrderDir =:= undefined, <<"desc">>, data_protocol:order_direction_to_json(OrderDir)),
    Query8 = db_query_builder:order_by(Query7, <<OrderByStr/binary, " ", OrderDirStr/binary>>),
    Query9 = db_query_builder:offset(Query8, Offset),
    Query10 = db_query_builder:limit(Query9, ?yesno(is_integer(Limit), min(Limit, ?query_limit), ?query_limit)),
    {Sql, Params} = db_query_builder:compile(Query10),
    logger:debug("SQL: ~s~nArgs: ~p~n", [Sql, Params], #{caption => ?MODULE}),
    {ok, Result} = db_query:select(Sql, Params),
    [web_protocol:crash_report_from_json(Log) || Log <- Result].

-spec get_crash_report_versions(App, Component, Branch) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Result :: [binary()].

get_crash_report_versions(App, Component, Branch) ->
    {ok, Versions} = db_if_crash_reports:versions(App, Component, Branch),
    Versions.

-spec get_crash_report_metadata_keys(App, Component, Branch, Versions) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Versions :: [binary()] | 'undefined',
    Result :: [binary()].

get_crash_report_metadata_keys(App, Component, Branch, Versions) ->
    Vs = web_util:trim_list(Versions, fun(E) -> util_binary:trim(web_util:sanitize_sql(E)) end),
    Vs1 = ?yesno(Vs =:= [], undefined, Vs),
    {ok, MetadataKeys} = db_if_crash_reports:metadata_keys(App, Component, Branch, Vs1),
    MetadataKeys.

-spec get_crash_report_pdb(App, Component, Branch, Version) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Result :: data_protocol:collection(binary()).

get_crash_report_pdb(App, Component, Branch, Version) ->
    Cards = [cards:card_application(Key) || Key <- card_config:applications()],
    AppCard = [Card || Card = #card_application{key = Key} <- Cards, Key =:= App],
    PdbFileLinks = case AppCard of
        [#card_application{default_pdb_source = DefaultPdb, pdb_sources = PdbPropList} | _] ->
            PdbForBranch = case lists:keyfind(Branch, 1, PdbPropList) of
                {Branch, PSK} -> PSK;
                _ -> undefined
            end,
            PdbSource = if
                is_atom(PdbForBranch), PdbForBranch =/= undefined -> cards:card_pdb_source(PdbForBranch);
                is_atom(DefaultPdb), DefaultPdb =/= undefined -> cards:card_pdb_source(DefaultPdb);
                true -> undefined
            end,
            case PdbSource of
                #card_pdb_source{source = Source, url = Url, auth_header = AuthHeader, auth_header_value = AuthHeaderValue} ->
                    UrlSuffix = ?yesno(Source =:= nginx_dir_index, "/", ""),
                    Url1 = interpolate_url(Url, App, Component, Branch, Version) ++ UrlSuffix,
                    AuthHeaders = auth_headers(AuthHeader, AuthHeaderValue),
                    Json = fetch_json(Url1, AuthHeaders, undefined),
                    case {Source, Json} of
                        {helios_api, #{<<"items">> := Items}} when is_list(Items) -> Items;
                        {nginx_dir_index, Items} when is_list(Items) -> [iolist_to_binary([Url1, Name]) || #{<<"name">> := Name} <- Items];
                        _ -> []
                    end;
                _ -> []
            end;
        _ -> []
    end,
    #collection{items = PdbFileLinks}.

%% Local functions

interpolate_url(Url, App, Component, Branch, Version) ->
    Url1 = binary:replace(Url, <<"{app}">>, App, [global]),
    Url2 = binary:replace(Url1, <<"{component}">>, Component, [global]),
    Url3 = binary:replace(Url2, <<"{branch}">>, Branch, [global]),
    Url4 = binary:replace(Url3, <<"{version}">>, Version, [global]),
    binary_to_list(Url4).

auth_headers(AuthHeader, AuthHeaderValue) when is_binary(AuthHeader), is_binary(AuthHeaderValue) ->
    [{binary_to_list(AuthHeader), binary_to_list(AuthHeaderValue)}];
auth_headers(_AuthHeader, _AuthHeaderValue) -> [].

fetch_json(Url, AuthHeaders, Default) ->
    try
        Request = {Url, [{"TE", "trailers"}] ++ AuthHeaders},
        {ok, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, Request, [], [{body_format, binary}]),
        web_util:decode_json_safe(Body, Default)
    catch _:_:_ -> Default
    end.
