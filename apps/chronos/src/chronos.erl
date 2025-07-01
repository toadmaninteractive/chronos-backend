-module(chronos).

%% Include files

-include_lib("db/include/db.hrl").

%% Exported functions

-export([
    start/0,
    stop/0,
    do_check/0,
    do_log/1
]).

%% API

-spec start() -> 'ok' | {'error', Reason::term()}.

start() ->
    aplib:start_app_recursive(chronos).

-spec stop() -> 'ok'.

stop() ->
    application:stop(chronos).

-spec do_check() -> boolean().

do_check() ->
    try
        case poolboy:status(?db_pool) of
            {ready, _, _, _} -> true;
            _ -> false
        end
    catch
        _:_:_ -> false
    end.

-spec do_log(#{app := App, component := Component, branch := Branch, version := Version, level := Level, timestamp := Timestamp, message := Message, data := Data}) -> Result when
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Level :: web_protocol:log_level(),
    Timestamp :: non_neg_integer(),
    Data :: jsx:json_term(),
    Message :: binary(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

do_log(#{app := App, component := Component, branch := Branch, version := Version, level := Level, timestamp := Timestamp, message := Message, data := Data}) ->
    DateTime = to_datetime_us(Timestamp),
    db_if_logs:create(App, Component, Branch, Version, DateTime, Level, Data, Message).

%% Local functions

to_datetime_us(Timestamp) ->
    Seconds = Timestamp div 1000000,
    Us = (Timestamp rem 1000000) / 1000000,
    {{Y, M, D}, {HH, MM, SS}} = time:seconds_to_datetime(Seconds),
    {{Y, M, D}, {HH, MM, SS + Us}}.
