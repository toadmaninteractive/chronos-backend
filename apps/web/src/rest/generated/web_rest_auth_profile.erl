%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_auth_profile).

-include_lib("igor/include/igor_http.hrl").

-behaviour(cowboy_handler).

-export([
    init/2,
    get_my_profile_403/1,
    get_my_profile_500/1
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

get_my_profile_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

get_my_profile_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_get(Req) ->
    try
        {ResponseContent, Req1} = web_rest_callback_auth:get_my_profile(Req),
        Body = jsx:encode(web_protocol:account_to_json(ResponseContent)),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req1)
    catch
        #{status_code := 403, response := ResponseContent403} ->
            ResponseContent403Body = jsx:encode(data_protocol:forbidden_error_ex_to_json(ResponseContent403, {custom, fun web_protocol:access_error_to_json/1})),
            ResponseHeaders403 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(403, ResponseHeaders403, ResponseContent403Body, Req);
        #{status_code := 500, response := ResponseContent500} ->
            ResponseContent500Body = jsx:encode(data_protocol:internal_server_error_to_json(ResponseContent500)),
            ResponseHeaders500 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(500, ResponseHeaders500, ResponseContent500Body, Req);
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

