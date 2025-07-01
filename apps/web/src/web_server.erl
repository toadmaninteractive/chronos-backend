-module(web_server).
-behaviour(gen_server).

%% Exported functions

-export([
    start_link/0
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    cowboy_pid :: pid()
}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_Args) ->
    self() ! init,
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(init, State) ->
    % Get settings
    {ok, BindIp} = web_config:bind_ip(),
    {ok, BindPort} = web_config:bind_port(),
    {ok, Acceptors} = web_config:acceptors(),
    {ok, Secure} = web_config:secure(),

    % Start Cowboy
    Pid = web_init:start_cowboy(BindIp, BindPort, Acceptors, Secure, true),
    {noreply, State#state{cowboy_pid = Pid}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions
