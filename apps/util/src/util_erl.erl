-module(util_erl).

%% Exported functions

-export([
    supervisor_spec/1,      % Create simple supervisor spec based on module name
    supervisor_spec/3       % Create simple supervisor spec based on module name, start function and parameters
]).

%% API

-spec supervisor_spec(Module :: module()) -> supervisor:child_spec().

supervisor_spec(Module) ->
    supervisor_spec(Module, start_link, []).

-spec supervisor_spec(Module :: module(), StartFun :: atom(), Params :: term()) -> supervisor:child_spec().

supervisor_spec(Module, StartFun, Params) ->
    #{
        id => Module,
        start => {Module, StartFun, Params},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [Module]
    }.

%% Local functions
