-module(logger_db_backend).

%% behaviour logger_backend

-export([adding_handler/1, removing_handler/1, log/2]).

-type handler_config() :: logger:handler_config() | #{
    chronos := #{
        app := string(),
        component := string(),
        branch := string(),
        version := string(),
        batch_size => pos_integer(),
        queue_limit => pos_integer()
    }
}.

-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

-define(delay_send, 10).            % 10ms
-define(flush_timeout, 5000).       % 5s

%% API: logger backend

-spec adding_handler(Config) -> {ok, Config2} | {error, Reason} when
        Config :: handler_config(),
        Config2 :: handler_config(),
        Reason :: term().

adding_handler(#{chronos := Chronos} = Config) ->
    % apply default config
    Chronos2 = maps:merge(#{
        % how many events to send in one go
        batch_size => 64,
        % cap event queue if sender stalled
        queue_limit => 1024
    }, Chronos),
    Config2 = Config#{chronos => Chronos2},
    % start engine
    {ok, Pid} = gen_server:start(?MODULE, Config2, []),
    {ok, Config2#{pid => Pid}}.

-spec removing_handler(Config) -> ok when
        Config :: handler_config().

removing_handler(#{pid := Pid} = _Config) ->
    gen_server:stop(Pid, normal, 5000).

-spec log(LogEvent, Config) -> ok when
        LogEvent :: logger:log_event(),
        Config :: handler_config().

log(LogEvent, #{pid := Pid} = _Config) ->
    gen_server:cast(Pid, {log, LogEvent}).

%% gen_server callbacks

init(#{chronos := Chronos} = Config) ->
    #{app := App, branch := Branch, component := Component, version := Version} = Chronos,
    State = #{
        config => Config#{
            chronos => Chronos#{
                app => util_binary:to_binary(App),
                branch => util_binary:to_binary(Branch),
                component => util_binary:to_binary(Component),
                version => util_binary:to_binary(Version)
            }
        },
        queue => queue:new()
    },
    {ok, State}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({log, LogEvent}, #{config := Config, queue := Queue} = State) ->
    State2 = schedule_flush(case application:ensure_started(db) of
        ok -> ?delay_send;
        _ -> ?flush_timeout
    end, State),
    %% NB: we cap the queue
    #{chronos := #{queue_limit := QueueLimit}} = Config,
    Queue2 = case queue:len(Queue) < QueueLimit of
        true ->
            queue:in(LogEvent, Queue);
        false ->
            Queue
    end,
    {noreply, State2#{queue => Queue2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #{config := Config, queue := Queue} = State) ->
    #{chronos := #{app := App, batch_size := BatchSize, branch := Branch, component := Component, version := Version}} = Config,
    % fetch a chunk of events
    {Chunk, Queue2} = queue:split(min(BatchSize, queue:len(Queue)), Queue),
    % send the chunk unless it's empty
    State2 = case queue:is_empty(Chunk) of
        false ->
            Logs = format_logs(queue:to_list(Chunk), Config),
            case db_if_logs:create_many(App, Component, Branch, Version, Logs) of
                {ok, _} ->
                    schedule_flush(?delay_send, State#{queue => Queue2});
                {error, _} ->
                    schedule_flush(?flush_timeout, State#{queue => Queue2})
            end;
        _ ->
            schedule_flush(?flush_timeout, State)
    end,
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Local functions

schedule_flush(Timeout, #{timer := Timer} = State) when is_reference(Timer) ->
    erlang:cancel_timer(Timer),
    schedule_flush(Timeout, State#{timer => reset});
schedule_flush(Timeout, State) ->
    State#{timer => erlang:send_after(Timeout, self(), flush)}.

format_logs(LogEvents, #{formatter := {FormatterModule, FormatterConfig}}) ->
    [
        begin
        #{data := Data, level := Level, time := Time} = logevent:normalize(LogEvent),
        {
            %% TODO: FIXME: time to datetime directly?
            iso8601:parse_datetimems(format_datetime(Time)),
            util_binary:to_binary(format_level(Level)),
            %% TODO: FIXME: this is ugly
            jsx:decode(util_binary:to_binary(zj:encode(Data))),
            util_binary:to_binary(FormatterModule:format(LogEvent, FormatterConfig)),
            1,
            1
        }
        end
        ||
        LogEvent <- LogEvents
    ].

format_level(critical) -> error;
format_level(alert) -> error;
format_level(emergency) -> fatal;
format_level(Else) -> Else.

%% NB: chronos takes iso8601 dates hence up to millisecond resolution
format_datetime(N) when is_integer(N) ->
    list_to_binary(calendar:system_time_to_rfc3339(N div 1000, [
        {offset, 0},
        {unit, millisecond}
    ])).

%% Local tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_datetime_test() ->
    ?assertEqual(
        <<"1970-01-01T00:00:00.000+00:00">>,
        format_datetime(0)
    ),
    ?assertEqual(
        <<"1970-01-02T00:00:00.000+00:00">>,
        format_datetime(86400 * 1000 * 1000)
    ),
    ok.

-endif.
