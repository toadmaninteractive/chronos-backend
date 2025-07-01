-module(web_check_api_key).

-behaviour(cowboy_handler).

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, #{api_key := ApiKey, next := Next} = Opts) ->
    case cowboy_req:header(<<"x-api-key">>, Req) of
        ApiKey ->
            % NB: do not leak used secret
            Next:init(Req, maps:without([api_key, next], Opts));
        _ ->
            % NB: both missing and non-matching api key means non-authenticated
            {ok, cowboy_req:reply(401, #{
                <<"WWW-Authenticate">> => <<"X-Api-Key header">>
            }, Req), Opts}
    end.

%% Local functions
