-module(cron_vacuum_crash_reports).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("web/include/card_protocol.hrl").

%% Exported functions

-export([
    run/0
]).

-define(query_limit, 1000).

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
    {ok, ActualApps} = db_if_crash_reports:apps(),

    % Loop over applications and delete outdated crash reports
    [begin
        ActualLifetime = maps:get(ActualApp, AppLifetimeMap, GlobalLifetime),
        case catch wipe_for_app(ActualApp, ActualLifetime) of
            {ok, 0} -> ignore;
            {ok, NumDeleted} -> logger:info("wiped ~B outdated crash reports for application <~s>", [NumDeleted, ActualApp], #{caption => ?MODULE});
            {error, Reason} -> logger:error("failed to wipe outdated crash reports for application <~s>, reason: ~p", [ActualApp, Reason], #{caption => ?MODULE})
        end
    end || ActualApp <- ActualApps],

    % Honor spec
    ok.

%% Local functions

wipe_for_app(App, LifetimeDays) ->
    wipe_for_app(App, LifetimeDays, 0).

wipe_for_app(App, LifetimeDays, AccCount) ->
    case db_if_crash_reports:get_expired(App, <<"id">>, <<"asc">>, 0, ?query_limit, LifetimeDays) of
        {ok, []} ->
            {ok, AccCount};
        {ok, Items} ->
            Ids = [Id || #{<<"id">> := Id} <- Items],
            {ok, _} = db_if_crash_reports:wipe_many(Ids),
            [?doif(is_binary(FileName) andalso FileName =/= <<>>, wipe_file(App, FileName)) || #{<<"filename">> := FileName} <- Items],
            wipe_for_app(App, LifetimeDays, AccCount + length(Items));
        {error, Reason} ->
            {error, Reason}
    end.

wipe_file(App, FileName) ->
    {ok, UploadsDir} = web_config:uploads_dir(),
    FilePath = filename:join([UploadsDir, App, FileName]),
    util_file:safe_delete(FilePath).
