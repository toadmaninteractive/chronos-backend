-module(web_mw_cors).

-behaviour(cowboy_middleware).

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
    {ok, Req1, Env}.

%% Local functions
