-module(web_ws).

-behaviour(cowboy_websocket).
-behaviour(igor_dispatcher).
-behaviour(service_chronos).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol_chronos.hrl").

%% Exported functions

%% cowboy_websocket callbacks

-export([
    init/2,
    websocket_handle/2,
    websocket_info/2,
    websocket_init/1
]).

%% igor_dispatcher callbacks

-export([
    format_unknown_error/1,
    handler/2,
    pop_context/3,
    push_context/3,
    send/3
]).

%% service_chronos callbacks

-export([
    on_log/7
]).

-define(ws_options, #{
    idle_timeout => 60000           % 60 sec
}).

-define(ws_ping_interval, 50000).   % 50 sec

%% API

%% cowboy_websocket callbacks

init(Req, #{api_key := ApiKey} = Opts) ->
    case cowboy_req:header(<<"x-api-key">>, Req) of
        ApiKey ->
            % NB: do not leak used secret
            {cowboy_websocket, Req, maps:without([api_key], Opts), ?ws_options};
        _ ->
            % NB: both missing and non-matching api key means non-authenticated
            {ok, cowboy_req:reply(401, #{
                <<"WWW-Authenticate">> => <<"X-Api-Key header">>
            }, Req), Opts}
    end.

websocket_init(State) ->
    self() ! heartbeat,
    {ok, State, hibernate}.

websocket_handle({text, Text}, State) ->
    case jsx:is_json(Text) of
        true ->
            Json = jsx:decode(Text, [return_maps]),
            case process_json(Json, State) of
                ok ->
                    ok;
                {error, Reason} ->
                    self() ! {send, text, Reason}
            end;
        false ->
            self() ! {send, text, invalid_data(json, #{})}
    end,
    {ok, State, hibernate};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

websocket_info({send, text, Msg}, State) ->
    % io:format("<====== RES ~p~n", [Msg]),
    {[{text, jsx:encode(Msg)}], State, hibernate};
websocket_info({send, binary, Binary}, State) ->
    {[{binary, Binary}], State, hibernate};
websocket_info(heartbeat, State) ->
    erlang:send_after(?ws_ping_interval, self(), heartbeat),
    {[ping], State, hibernate};
websocket_info(disconnect, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State, hibernate}.

%% igor_dispatcher callbacks

-spec format_unknown_error(Exception) -> Reply when
      Exception :: term(),
      Reply :: binary().

format_unknown_error({error, Reason, StackTrace}) ->
    logger:error(#{msg => format_unknown_error, error => error, reason => Reason}, #{stacktrace => StackTrace, caption => ?MODULE}),
    FormatError = io_lib:format("~p:~p", [error, Reason]),
    iolist_to_binary(FormatError);
format_unknown_error({throw, Exception}) ->
    logger:error(#{msg => format_unknown_error, error => unknown_rpc_error, reason => Exception}, #{caption => ?MODULE}),
    Reply = iolist_to_binary(io_lib:format("~p", [Exception])),
    Reply.

-spec send(State, Service, Packet) -> State when
      State :: state:state(),
      Service :: protocol_services:service_id(),
      Packet :: igor_json:json().

send(#{proc := Proc} = _State, service_chronos, Packet) ->
    Json = service_chronos:s2c_message_to_json(Packet),
    Proc ! {send, text, Json}.

-spec handler(State, Service) -> Handler when
      State :: state:state(),
      Service :: protocol_services:service_id(),
      Handler :: module().

handler(_State, service_chronos) ->
    ?MODULE.

-spec push_context(State, Service, Context) -> {ContextId, State} when
      State :: state:state(),
      Service :: protocol_services:service_id(),
      Context :: igor_types:context(),
      ContextId :: igor_types:context_id().

push_context(_State, service_chronos, _Context) ->
    throw(not_implemented).

-spec pop_context(State, Service, ContextId) -> {Context, State} when
      State :: state:state(),
      Service :: protocol_services:service_id(),
      ContextId :: igor_types:context_id(),
      Context :: igor_types:context().

pop_context(_State, service_chronos, _ContextId) ->
    throw(not_implemented).

%% service_chronos callbacks

-define(chunk_size, 100).

-spec on_log(State, RpcId, App, Component, Branch, Version, Logs) -> State when
      RpcId :: igor_types:rpc_id(),
      App :: binary(),
      Component :: binary(),
      Branch :: binary(),
      Version :: binary(),
      Logs :: [protocol_chronos:log_entry()].

on_log(State, RpcId, App, Component, Branch, Version, Logs) ->
    % check app, component, branch and version
    ?doif(util_binary:trim(App) =:= <<>>, throw(#invalid_data{reason = <<"app">>})),
    ?doif(util_binary:trim(Component) =:= <<>>, throw(#invalid_data{reason = <<"component">>})),
    ?doif(util_binary:trim(Branch) =:= <<>>, throw(#invalid_data{reason = <<"branch">>})),
    ?doif(util_binary:trim(Version) =:= <<>>, throw(#invalid_data{reason = <<"version">>})),
    % check log entries
    ?doif(length(Logs) =:= 0, throw(#invalid_data{reason = <<"empty">>})),

    % group log entries to chunks
    Chunks = util_lists:to_chunks([
        % NB: allow only maps as data
        {TimeStamp, Level, ?yesno(is_map(Data), Data, #{}), Message, MsgCount, SeqId}
        ||
        #log_entry{
            data = Data,
            level = Level,
            message = Message,
            msg_count = MsgCount,
            seq_id = SeqId,
            timestamp = TimeStamp
        } <- Logs
    ], ?chunk_size),

    % save groups of log entries to database
    Result = [db_if_logs:create_many(App, Component, Branch, Version, Chunk) || Chunk <- Chunks],

    Loaded = lists:foldl(fun
        ({ok, N}, Acc) -> Acc + N;
        ({error, _}, Acc) -> Acc
    end, 0, Result),
    service_chronos:reply_log(State, RpcId, Loaded),

    State.

%% Local functions

process_json(#{<<"method">> := Method, <<"id">> := RpcId, <<"params">> := _Params} = Json, State) ->
    try
        % TODO: FIXME: service_chronos:s2c_message_from_json() throws at unknown method, ok|error would be nicer
        Msg = service_chronos:c2s_message_from_json(Json),
        % io:format(">====== REQ ~p~n", [Msg]),
        service_chronos:recv(Msg, State#{proc => self()}),
        ok
    catch
        _:_:_ ->
            {error, invalid_data(params, #{<<"method">> => Method, <<"id">> => RpcId})}
    end;
process_json(_, _State) ->
    {error, invalid_data(method, #{})}.

%% TODO: FIXME: missing service_chronos:s2c_message_to_json() for unknown method

invalid_data(Reason, Base) ->
    Base#{
        <<"error">> => #{
            <<"code">> => 2,
            <<"message">> => <<"InvalidData">>,
            <<"data">> => protocol_chronos:invalid_data_to_json(#invalid_data{
                reason = util_binary:to_binary(Reason)
            })
        }
    }.
