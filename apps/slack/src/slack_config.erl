-module(slack_config).

%% Exported functions

-export([
    api_url/0,
    team_id/0,
    token/0
]).

%% API

-spec api_url() -> binary().

api_url() ->
    {ok, ApiUrl} = application:get_env(slack, api_url),
    util_binary:to_binary(ApiUrl).

-spec team_id() -> binary().

team_id() ->
    {ok, TeamId} = application:get_env(slack, team_id),
    util_binary:to_binary(TeamId).

-spec token() -> string().

token() ->
    {ok, Token} = application:get_env(slack, token),
    Token.

%% Local functions
