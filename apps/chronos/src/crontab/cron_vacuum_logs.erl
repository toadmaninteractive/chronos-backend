-module(cron_vacuum_logs).

%% Include files

-include_lib("web/include/card_protocol.hrl").

%% Exported functions

-export([
    run/0
]).

%% API

-spec run() -> 'ok'.

run() ->
    % Get global and per-application lifetime and construct app lifetime map
    GlobalLifetime = card_config:log_lifetime_days(),
    AppLifetimeMap = lists:foldl(fun(AppKey, Acc) ->
        #card_application{key = App, log_lifetime_days = Lifetime} = cards:card_application(AppKey),
        App1 = util_binary:trim(util_binary:to_lower(App)),
        Acc#{App1 => Lifetime}
    end, #{}, card_config:applications()),
    AppLifetimeMap,

    % Get actual applications from database
    {ok, ActualApps} = db_if_logs:apps(),

    % Loop over applications and delete outdated logs
    [begin
        ActualLifetime = maps:get(ActualApp, AppLifetimeMap, GlobalLifetime),
        case db_if_logs:wipe(ActualApp, ActualLifetime) of
            {ok, 0} -> ignore;
            {ok, NumDeleted} -> logger:info("wiped ~B outdated logs for application <~s>", [NumDeleted, ActualApp], #{caption => ?MODULE});
            {error, Reason} -> logger:error("failed to wipe outdated logs for application <~s>, reason: ~p", [ActualApp, Reason], #{caption => ?MODULE})
        end
    end || ActualApp <- ActualApps],

    % Honor spec
    ok.

%% Local functions
