-module(slack_bot).
-behaviour(gen_server).

%% Include files

%% -include_lib("stdlib/include/ms_transform.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include_lib("web/include/card_protocol.hrl").
-include_lib("web/include/web_protocol.hrl").
-include("slack_protocol.hrl").

%% Exported functions

-export([
    start_link/0,
    user_id/1,
    user/1
]).

-define(slack_supervisor, slack_sup).
-define(slack_receiver, slack_receiver).

-define(check_new_logs_msg, check_new_logs).    % Message
-define(check_new_logs_interval, 100).          % 100 msec

-define(update_users_msg, update_users).        % Message
-define(update_users_retry_interval, 60000).    % 1 min
-define(update_users_interval, 60 * 60 * 1000). % 1 hour

-define(ets_users, slack_bot_users).
-define(ets_members, slack_bot_members).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    last_id = 0 :: non_neg_integer(),
    pid_users :: pid() | 'undefined',
    subscribed = false :: boolean()
}).

-record(kv, {
    key :: term(),
    value :: term()
}).

-record(user, {
    id :: binary(),
    display_name :: binary(),
    real_name :: binary()
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec user_id(Name) -> Result when
    Name :: binary(),
    Result :: binary().

user_id(Name) ->
    case ets:lookup(?ets_users, slack_util:to_lower_trim(Name)) of
        [#kv{value = UserId} | _] -> UserId;
        _ -> undefined
    end.

-spec user(Id) -> Result when
    Id :: binary(),
    Result :: #user{}.

user(Id) ->
    case ets:lookup(?ets_members, Id) of
        [#user{} = User | _] -> User;
        _ -> undefined
    end.

%% gen_server callbacks

init(_) ->
    erlang:send_after(0, self(), init),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(init, State) ->
    ets:new(?ets_users, [set, public, named_table, {keypos, #kv.key}]),
    ets:new(?ets_members, [set, public, named_table, {keypos, #user.id}]),
    erlang:send_after(0, self(), ?update_users_msg),
    {ok, LastId} = db_if_logs:last_id(),
    {noreply, State#state{last_id = LastId}};

handle_info(?update_users_msg, State) ->
    Pid = proc_lib:spawn(fun update_slack_users/0),
    erlang:monitor(process, Pid),
    {noreply, State#state{pid_users = Pid}};

handle_info(?check_new_logs_msg, #state{last_id = LastId} = State) ->
    {ok, Logs} = db_if_logs:get_after(LastId),  % TODO: maybe do in parts
    [begin
        ExactMatchKey = {App, Component, Branch},
        WildcardMatchKey = {App, Component, undefined},
        gproc:send({p, l, ExactMatchKey}, {ExactMatchKey, LogEntry}),
        gproc:send({p, l, WildcardMatchKey}, {WildcardMatchKey, LogEntry})
    end || #log_entry{app = App, component = Component, branch = Branch} = LogEntry <- Logs],
    LastId1 = case Logs of
        [] -> LastId;
        Items -> lists:max([Id || #log_entry{id = Id} <- Items])
    end,
    erlang:send_after(?check_new_logs_interval, self(), ?check_new_logs_msg),
    {noreply, State#state{last_id = LastId1}};

handle_info({catalogue, _CardKey}, State) ->
    rearm_receivers(),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, normal}, #state{pid_users = Pid, subscribed = Subscribed} = State) ->
    rearm_receivers(),
    ?doif(not Subscribed, catalogue:subscribe()),
    erlang:send_after(?update_users_interval, self(), ?update_users_msg),
    erlang:send_after(0, self(), ?check_new_logs_msg),
    {noreply, State#state{pid_users = undefined, subscribed = true}};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, #state{pid_users = Pid} = State) ->
    logger:error("failed to get Slack users, reason: ~p", [Reason], #{caption => ?MODULE}),
    erlang:send_after(?update_users_retry_interval, self(), ?update_users_msg),
    {noreply, State#state{pid_users = undefined}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

display_name(Id) ->
    case user(Id) of
        #user{display_name = <<>>, real_name = RealName} -> RealName;
        #user{display_name = DisplayName} -> DisplayName
    end.

real_name(Id) ->
    #user{real_name = RealName} = user(Id),
    RealName.

worker_fun(person) -> start_link_person;
worker_fun(channel) -> start_link_channel.

worker_params(person, Id) -> [Id, display_name(Id), real_name(Id)];
worker_params(channel, Id) -> [Id].

worker_spec(Kind, Id) ->
    #{
        id => {?slack_receiver, {Kind, Id}},
        start => {?slack_receiver, worker_fun(Kind), worker_params(Kind, Id)},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => dynamic
    }.

cleanup_users(SlackUsers) ->
    UserKeys = lists:foldl(fun(#member{profile = #member_profile{display_name = DisplayName, real_name = RealName}}, Acc) ->
        Acc1 = ordsets:add_element(slack_util:to_lower_trim(DisplayName), Acc),
        ordsets:add_element(slack_util:to_lower_trim(RealName), Acc1)
    end, ordsets:new(), SlackUsers),
    ExistingKeys = ets:foldl(fun(#kv{key = Key}, AccSet) -> ordsets:add_element(Key, AccSet) end, ordsets:new(), ?ets_users),
    [begin
        ets:delete(?ets_users, Key),
        logger:info("Slack user key <~s> removed", [Key], #{caption => ?MODULE})
    end || Key <- ordsets:subtract(ExistingKeys, UserKeys)].

cleanup_members(SlackUsers) ->
    MemberKeys = ordsets:from_list([Id || #member{id = Id} <- SlackUsers]),
    ExistingKeys = ets:foldl(fun(#user{id = Id}, AccSet) -> ordsets:add_element(Id, AccSet) end, ordsets:new(), ?ets_members),
    [begin
        ets:delete(?ets_members, Key),
        logger:info("Slack member key <~s> removed", [Key], #{caption => ?MODULE})
    end || Key <- ordsets:subtract(ExistingKeys, MemberKeys)].

create_users(SlackUsers) ->
    [begin

        ?doif(DisplayName =/= <<>>, ets:insert(?ets_users, #kv{key = slack_util:to_lower_trim(DisplayName), value = UserId})),
        ets:insert(?ets_users, #kv{key = slack_util:to_lower_trim(RealName), value = UserId}),
        ets:insert(?ets_members, #user{id = UserId, display_name = DisplayName, real_name = RealName})
    end || #member{id = UserId, profile = #member_profile{display_name = DisplayName, real_name = RealName}} <- SlackUsers].

update_slack_users() ->
    SlackUsers = slack:get_users(),
    cleanup_users(SlackUsers),
    cleanup_members(SlackUsers),
    create_users(SlackUsers).

slack_receivers() ->
    AppKeys = card_config:applications(),
    Apps = [cards:card_application(Key) || Key <- AppKeys],
    CompKeys = [Keys || #card_application{components = Keys} <- Apps],
    Components = [cards:card_component(Key) || Key <- lists:usort(lists:flatten(CompKeys))],
    Receivers = [Receivers || #card_component{slack_receivers = Receivers} <- Components],
    ReceiverTuples = [{Kind, slack_util:to_lower_trim(Title)} || #slack_receiver{kind = Kind, title = Title} <- lists:flatten(Receivers)],
    lists:usort(ReceiverTuples).

rearm_receivers() ->
    % Get Slack receivers from application component cards
    Receivers = slack_receivers(),

    % Split receivers into valid and invalid
    {ValidReceivers, InvalidReceivers} = lists:foldr(fun
        ({channel, ChannelTitle}, {AccValid, AccInvalid}) ->
            {[{channel, ChannelTitle} | AccValid], AccInvalid};

        ({person, Name}, {AccValid, AccInvalid}) ->
            case user_id(Name) of
                undefined -> {AccValid, [{person, Name} | AccInvalid]};
                UserId -> {[{person, UserId} | AccValid], AccInvalid}
            end
    end, {[], []}, Receivers),

    % Log non-existing receivers
    [logger:error("invalid receiver person: ~s", [Name], #{caption => ?MODULE}) || {person, Name} <- InvalidReceivers],

    % Loop thru Slack application supervisor children: start or stop if necessary
    Children = supervisor:which_children(?slack_supervisor),
    SlackWorkers = [{Kind, Id} || {{?slack_receiver, {Kind, Id}}, _, _, _} <- Children],

    % Turn lists into sets and check which workers to start or to stop
    NewSet = ordsets:from_list(ValidReceivers),
    SupervisedSet = ordsets:from_list(SlackWorkers),
    StartTheseChildren = ordsets:subtract(NewSet, SupervisedSet),
    StopTheseChildren = ordsets:subtract(SupervisedSet, NewSet),

    % Stop children
    [begin
        ChildId = {?slack_receiver, ChildKey},
        supervisor:terminate_child(?slack_supervisor, ChildId),
        supervisor:delete_child(?slack_supervisor, ChildId)
    end || ChildKey <- StopTheseChildren],

    % Start children
    [supervisor:start_child(?slack_supervisor, worker_spec(Kind, Title)) || {Kind, Title} <- StartTheseChildren].
