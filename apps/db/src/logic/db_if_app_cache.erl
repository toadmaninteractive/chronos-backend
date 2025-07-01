-module(db_if_app_cache).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    structure/0
]).

%% API

-spec structure() ->
    {'ok', [maps:map()]} | {'error', Reason :: atom()}.

structure() ->
    Query = <<
        "SELECT app, component, branch, version FROM acbv_logs ",
        "UNION ",
        "SELECT app, component, branch, version FROM acbv_crash_reports ",
        "GROUP BY app, component, branch, version ",
        "ORDER BY app, component, branch, version"
    >>,
    case db_query:select(Query, []) of
        {ok, Items} -> {ok, Items};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
