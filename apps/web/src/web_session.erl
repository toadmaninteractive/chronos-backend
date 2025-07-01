-module(web_session).
-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("db/include/protocol.hrl").
%% -include("settings.hrl").
-include("session.hrl").
-include("kv.hrl").

%% Exported functions

-export([
    start_link/0,
    get/2,
    create/2,
    prolong/2,
    delete/2,
    delete_for/2,
    encode/2,
    decode/1
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

-define(refresh_interval, 10*1000). % 10 sec
-define(ets_sessions, web_session).

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(UserType :: 'personnel', SessionId :: non_neg_integer()) ->
    {'ok', session()} | {'error', 'not_exists'}.

get(?actor_personnel, SessionId) ->
    case ets:lookup(?ets_sessions, {?actor_personnel, SessionId}) of
        [#session{} = Session|_] -> {ok, Session};
        _ -> {error, not_exists}
    end.

-spec create(UserType :: 'personnel', UserId :: non_neg_integer()) ->
    {'ok', SessionId :: binary()}.

create(?actor_personnel, UserId) ->
    % Generate unique session ID
    SessionId = web_util:uuid64(),
    Now = util_time:utc_seconds(),

    % Get session litetime and store session in database / ETS
    {ok, Lifetime} = db_if_settings:personnel_session_duration(),
    ok = db_if_personnel_sessions:create(SessionId, UserId, Lifetime),
    ok = resume(?actor_personnel, SessionId, UserId, Now, Now + Lifetime),
    {ok, SessionId}.

-spec prolong(UserType :: 'personnel', SessionId :: binary()) ->
    'ok' | {'error', 'not_exists'}.

prolong(?actor_personnel, SessionId) ->
    case get(?actor_personnel, SessionId) of
        {ok, Session} ->
            % Get session litetime and update session in ETS and database
            Now = util_time:utc_seconds(),
            {ok, Lifetime} = db_if_settings:personnel_session_duration(),
            ets:insert(?ets_sessions, Session#session{valid_thru = Now + Lifetime}),
            db_if_personnel_sessions:prolong(SessionId, Lifetime);
        {error, _Reason} = Error ->
            Error
    end.

-spec delete(UserType :: 'personnel', SessionId :: binary()) ->
    'ok' | {'error', 'not_exists'}.

delete(?actor_personnel, SessionId) ->
    case get(?actor_personnel, SessionId) of
        {ok, #session{key = Key}} ->
            % Delete from ETS and database
            ets:delete(?ets_sessions, Key),
            db_if_personnel_sessions:delete(SessionId);
        {error, _Reason} = Error ->
            Error
    end.

-spec delete_for(UserType :: 'personnel', UserId :: non_neg_integer() | ?null) ->
    'ok'.

delete_for(?actor_personnel, UserId) ->
    % Delete all sessions in ETS
    MatchSpecDelete = ets:fun2ms(fun(#session{key = SessionKey}) when SessionKey =:= {?actor_personnel, UserId} -> true end),
    ets:select_delete(?ets_sessions, MatchSpecDelete),
    {ok, _} = db_if_personnel_sessions:delete_for(UserId),
    ok.

-spec encode(UserType :: 'personnel', SessionId :: binary()) ->
    {'ok', EncodedSession :: binary()} | {'error', 'jwk_not_loaded'}.

encode(?actor_personnel, SessionId) ->
    try
        JWK = web_storage:jwk(),
        JsonStr = web_util:encode_json(#{user_type => ?actor_personnel, session_id => SessionId}),
        EncodedSession = jws:encode_compact(JsonStr, #{alg => <<"RS256">>}, JWK),
        {ok, EncodedSession}
    catch _Type:_What:_StackTrace ->
        {error, jwk_not_loaded}
    end.

-spec decode(EncodedSession :: binary()) ->
    {'ok', Session :: session()} | {'error', 'jwk_not_loaded' | 'not_exists' | 'invalid'}.

decode(EncodedSession) ->
    try
        % Decode session in a safe manner
        JWK = web_storage:jwk(),
        Result = try
            {true, JsonStr, _} = jws:decode_compact(EncodedSession, JWK),
            web_util:decode_json_safe(JsonStr, undefined)
        catch
            _C:_R:_S -> undefined
        end,
        DecodedResult = case Result of
            #{<<"user_type">> := <<"personnel">>, <<"session_id">> := Sid} -> {?actor_personnel, Sid};
            _ -> {error, invalid}
        end,
        case DecodedResult of
            {error, invalid} ->
                {error, invalid};
            {UserType, SessionId} ->
                case get(UserType, SessionId) of
                    {ok, #session{} = Session} -> {ok, Session};
                    {error, not_exists} -> {error, not_exists}
                end
        end
    catch _Type:_What:_StackTrace ->
        {error, jwk_not_loaded}
    end.

%% gen_server callbacks

init(_Args) ->
    % Create ETS tables, cleanup expires and preload valid sessions
    ets:new(?ets_sessions, [set, public, named_table, {keypos, #session.key}]),
    cleanup_sessions(),
    preload_sessions(),
    erlang:send_after(?refresh_interval, self(), refresh),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(refresh, State) ->
    % Clean up expired sessions in ETS
    Now = util_time:utc_seconds(),
    MatchSpecDelete = ets:fun2ms(fun(#session{valid_thru = ValidThru}) when ValidThru < Now -> true end),
    ets:select_delete(?ets_sessions, MatchSpecDelete),

    % Clean up expired sessions in database
    cleanup_sessions(),

    % Repeat refresh after certain interval
    erlang:send_after(?refresh_interval, self(), refresh),
    {noreply, State};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

cleanup_sessions() ->
    % Clean up expired sessions in database
    db_if_personnel_sessions:cleanup_expired().

preload_sessions() ->
    % Load personnel sessions from database and resume them
    case db_if_personnel_sessions:get() of
        {ok, ANS} when is_list(ANS) ->
            [resume(personnel, SessionId, PersonnelId, CreatedAt, ValidThru) || #{
                <<"id">> := SessionId,
                <<"personnel_id">> := PersonnelId,
                <<"created_at">> := CreatedAt,
                <<"valid_thru">> := ValidThru
            } <- ANS];
        {error, _} -> ignore
    end.

resume(?actor_personnel, SessionId, UserId, CreatedAt, ValidThru) ->
    Session = #session{
        key = {?actor_personnel, SessionId},
        user_id = UserId,
        created_at = CreatedAt,
        valid_thru = ValidThru
    },
    ets:insert(?ets_sessions, Session),
    ok.
