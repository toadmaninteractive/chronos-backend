-module(postgres_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% Exported functions

-export([
    start_link/1
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
    connection :: pid() | 'undefined'
}).

%% API

-spec start_link(Args :: term()) ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% gen_server callbacks

init(Args) ->
    self() ! {init, Args},
    {ok, #state{}}.

handle_call({squery, Sql}, _From, #state{connection = Connection} = State) ->
    Reply = epgsql:squery(Connection, Sql),
    {reply, Reply, State};

handle_call({equery, Statement, Params}, _From, #state{connection = Connection} = State) ->
    Reply = epgsql:equery(Connection, Statement, Params),
    {reply, Reply, State};

handle_call({transaction, Variant}, _From, #state{connection = Connection} = State) ->
    Reply = try
        {ok, _, _} = epgsql:squery(Connection, <<"BEGIN">>),
        Result = case Variant of
            Fun when is_function(Fun, 1) -> Fun(Connection);
            Sql when is_binary(Sql) -> epgsql:squery(Connection, Sql);
            {Sql, Params} when is_binary(Sql), is_list(Params) -> epgsql:equery(Connection, Sql, Params);
            _ -> {error, badarg}
        end,
        {ok, _, _} = epgsql:squery(Connection, <<"COMMIT">>),
        Result
    catch
        _:Reason ->
            epgsql:squery(Connection, <<"ROLLBACK">>),
            {rollback, Reason}
    end,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info({init, Args}, State) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    {ok, Connection} = epgsql:connect(Hostname, Username, Password, #{database => Database, timeout => db_config:connect_timeout()}),
    {noreply, State#state{connection = Connection}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, #state{connection = Connection}) ->
    ok = epgsql:close(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
