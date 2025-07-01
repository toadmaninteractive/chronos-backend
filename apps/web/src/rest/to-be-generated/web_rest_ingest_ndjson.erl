-module(web_rest_ingest_ndjson).

-behaviour(cowboy_handler).

%% Include files

-include_lib("igor/include/igor_http.hrl").

%% Exported functions

-export([
    init/2,
    ingest_logs_as_ndjson_400/1,
    ingest_logs_as_ndjson_403/1,
    ingest_logs_as_ndjson_500/1
]).

%% API

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"POST">>, Req) ->
    case cowboy_req:has_body(Req) of
        true -> handle_post(Req);
        false -> cowboy_req:reply(400, Req)
    end;
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"POST">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

ingest_logs_as_ndjson_400(ResponseContent400) ->
    throw(#{status_code => 400, response => ResponseContent400}).

ingest_logs_as_ndjson_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

ingest_logs_as_ndjson_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

%% Local functions

handle_post(Req) ->
    try
        App = cowboy_req:binding(app, Req),
        Component = cowboy_req:binding(component, Req),
        Branch = cowboy_req:binding(branch, Req),
        Version = cowboy_req:binding(version, Req),
        {ResponseContent, Req1} = web_rest_callback_ingest_ndjson:ingest_logs_as_ndjson(Req, App, Component, Branch, Version),
        Body = jsx:encode(data_protocol:generic_response_to_json(ResponseContent)),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(201, ResponseHeaders, Body, Req1)
    catch
        #{status_code := 400, response := ResponseContent400} ->
            ResponseContent400Body = jsx:encode(data_protocol:bad_request_error_to_json(ResponseContent400, {custom, fun ingestion_protocol:ingestion_error_to_json/1})),
            ResponseHeaders400 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(400, ResponseHeaders400, ResponseContent400Body, Req);
        #{status_code := 403, response := ResponseContent403} ->
            ResponseContent403Body = jsx:encode(data_protocol:forbidden_error_to_json(ResponseContent403)),
            ResponseHeaders403 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(403, ResponseHeaders403, ResponseContent403Body, Req);
        #{status_code := 500, response := ResponseContent500} ->
            ResponseContent500Body = jsx:encode(data_protocol:internal_server_error_to_json(ResponseContent500)),
            ResponseHeaders500 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(500, ResponseHeaders500, ResponseContent500Body, Req);
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.
