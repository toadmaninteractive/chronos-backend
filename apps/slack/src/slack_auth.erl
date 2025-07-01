-module(slack_auth).

%% Exported functions

-export([
    default_headers/0
]).

%% API

-spec default_headers() -> proplists:proplist().

default_headers() ->
    Token = slack_config:token(),
    [{"Authorization", "Bearer " ++ Token}].

%% Local functions
