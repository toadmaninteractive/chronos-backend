-module(db_sup).
-behaviour(supervisor).

%% Include files

-include("db.hrl").

%% Exported functions

-export([
    start_link/0
]).

%% supervisor callbacks

-export([
    init/1
]).

%% API

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

init(_Args) ->
    WorkerArgs = [
        {hostname, db_config:hostname()},
        {database, db_config:database()},
        {username, db_config:username()},
        {password, db_config:password()}
    ],
    PoolArgs = [
        {name, {local, ?db_pool}},
        {worker_module, postgres_worker},
        {size, db_config:pool_size()},
        {max_overflow, db_config:pool_max_overflow()}
    ],
    PoolSpecs = [poolboy:child_spec(?db_pool, PoolArgs, WorkerArgs)],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
