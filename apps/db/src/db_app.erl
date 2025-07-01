-module(db_app).
-behaviour(application).

%% Exported functions

-export([
    start/2,
    stop/1
]).

%% API

start(_Type, _StartArgs) ->
    case maybe_migrate() of
        ok -> db_sup:start_link();
        {error, Reason} -> {error, Reason}
    end.

stop(_State) ->
    ok.

%% Local functions

maybe_migrate() ->
    case db_config:auto_migrate() of
        false ->
            ok;
        true ->
            {ok, LastVersion} = db_config:last_version(),
            case db_helper:actualize_db() of
                {ok, LastVersion} -> ok;
                {ok, _} -> {error, db_version_mismatch};
                {error, Reason} -> {error, Reason}
            end
    end.
