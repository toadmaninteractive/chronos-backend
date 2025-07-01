-module(slack_sup).
-behaviour(supervisor).

%% Exported functions

-export([
    start_link/0
]).

%% supervisor callbacks

-export([
    init/1
]).

%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 1000, 3600}, [
        util_erl:supervisor_spec(slack_bot)
    ]}}.

%% Local functions
