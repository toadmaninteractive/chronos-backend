%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_crash_reports).

-include_lib("igor/include/igor_http.hrl").

-behaviour(cowboy_handler).

-export([
    init/2
]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"GET">>, Req) ->
    handle_get(Req);
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"GET">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

handle_get(Req) ->
    try
        App = cowboy_req:binding(app, Req),
        Component = cowboy_req:binding(component, Req),
        Branch = cowboy_req:binding(branch, Req),
        Qs = cowboy_req:parse_qs(Req),
        Versions = igor_http:parse_query(<<"version">>, Qs, {option, {list, <<",">>, string}}),
        DateFrom = igor_http:parse_query(<<"date_from">>, Qs, {option, {custom, fun web_types:datetime_from_text/1}}),
        DateTo = igor_http:parse_query(<<"date_to">>, Qs, {option, {custom, fun web_types:datetime_from_text/1}}),
        Search = igor_http:parse_query(<<"search">>, Qs, {option, string}),
        GameEngine = igor_http:parse_query(<<"game_engine">>, Qs, {option, string}),
        Username = igor_http:parse_query(<<"username">>, Qs, {option, string}),
        Reason = igor_http:parse_query(<<"reason">>, Qs, {option, string}),
        OrderBy = igor_http:parse_query(<<"order_by">>, Qs, {option, {custom, fun web_protocol:crash_report_order_by_from_string/1}}),
        OrderDir = igor_http:parse_query(<<"order_dir">>, Qs, {option, {custom, fun data_protocol:order_direction_from_string/1}}),
        Offset = igor_http:parse_query(<<"offset">>, Qs, {option, int}),
        Limit = igor_http:parse_query(<<"limit">>, Qs, {option, int}),
        Response = web_rest_callback_crash_reports:get_crash_reports(App, Component, Branch, Versions, DateFrom, DateTo, Search, GameEngine, Username, Reason, OrderBy, OrderDir, Offset, Limit),
        Body = jsx:encode(igor_json:pack_value(Response, {list, {custom, fun web_protocol:crash_report_to_json/1}})),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req)
    catch
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

