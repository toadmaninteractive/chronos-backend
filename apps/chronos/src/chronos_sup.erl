-module(chronos_sup).
-behaviour(supervisor).

%% Exported functions

-export([
    start_link/0
]).

%% Supervisor callbacks

-export([
    init/1
]).

%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 1000, 3600}, []}}.

%% Local functions
