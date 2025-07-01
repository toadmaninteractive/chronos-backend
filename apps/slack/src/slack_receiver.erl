-module(slack_receiver).
-behaviour(gen_server).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("web/include/card_protocol.hrl").
-include_lib("web/include/web_protocol.hrl").
-include("slack_protocol.hrl").

%% Exported functions

-export([
    start_link_person/3,
    start_link_channel/1
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
    kind :: 'person' | 'channel',
    id :: binary(),
    display_name :: binary() | 'undefined',
    real_name :: binary() | 'undefined',
    subscription_tab :: atom(),
    message_tab :: atom()
}).

-record(subscription, {
    message_key :: tuple(),
    min_log_level :: web_protocol:log_level(),
    throttle_sec :: non_neg_integer(),
    collapse_fields :: [binary()]
}).

-record(cached_message, {
    key :: {MsgKey :: tuple(), CollapseKey :: map()},
    event :: web_protocol:log_entry(),
    from :: non_neg_integer(),
    to :: non_neg_integer(),
    count :: non_neg_integer(),
    throttle_sec :: non_neg_integer()
}).

-define(init_msg, init).
-define(check_msg, check).
-define(check_msg_interval, 1000).
-define(migrate_subscription_msg, migrate_subscription).
-define(ets_subscriptions, subscriptions).
-define(ets_cached_messages, cached_messages).

%% API

start_link_person(Id, DisplayName, RealName) ->
    ChildName = list_to_atom(lists:flatten(io_lib:format("~s_person_~s", [?MODULE, Id]))),
    gen_server:start_link({local, ChildName}, ?MODULE, [person, Id, DisplayName, RealName], []).

start_link_channel(Id) ->
    ChildName = list_to_atom(lists:flatten(io_lib:format("~s_channel_~s", [?MODULE, Id]))),
    gen_server:start_link({local, ChildName}, ?MODULE, [channel, Id], []).

%% gen_server callbacks

init([channel, Id]) ->
    SubTab = ets_tab(channel, Id, ?ets_subscriptions),
    MessageTab = ets_tab(channel, Id, ?ets_cached_messages),
    logger:debug("channel #~s with pid ~p started", [Id, self()], #{caption => ?MODULE}),
    erlang:send_after(0, self(), ?init_msg),
    {ok, #state{kind = channel, id = Id, subscription_tab = SubTab, message_tab = MessageTab}};

init([person, Id, DisplayName, RealName]) ->
    SubTab = ets_tab(person, Id, ?ets_subscriptions),
    MessageTab = ets_tab(person, Id, ?ets_cached_messages),
    logger:debug("person @~s <~s> with pid ~p started", [DisplayName, Id, self()], #{caption => ?MODULE}),
    erlang:send_after(0, self(), ?init_msg),
    {ok, #state{kind = person, id = Id, display_name = DisplayName, real_name = RealName, subscription_tab = SubTab, message_tab = MessageTab}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(?init_msg, #state{kind = Kind, id = Id, display_name = DisplayName, real_name = RealName, subscription_tab = SubTab, message_tab = MsgTab} = State) ->
    ets:new(SubTab, [set, public, named_table, {keypos, #subscription.message_key}]),
    ets:new(MsgTab, [set, public, named_table, {keypos, #cached_message.key}]),
    catalogue:subscribe(),
    rearm_subscriptions(SubTab, MsgTab, Kind, Id, DisplayName, RealName),
    erlang:send_after(?check_msg_interval, self(), ?check_msg),
    {noreply, State};

handle_info(?check_msg, #state{id = Id, message_tab = MsgTab} = State) ->
    Now = time:seconds(),
    ets:foldl(fun
        (#cached_message{key = Key, to = To, event = LogEvent, count = MsgCount}, Acc) when To =< Now ->
            ets:delete(MsgTab, Key),
            ?doif(MsgCount > 0, send_message(Id, LogEvent, MsgCount)),
            Acc + 1;
        (_, Acc) -> Acc
    end, 0, MsgTab),
    erlang:send_after(?check_msg_interval, self(), ?check_msg),
    {noreply, State};

handle_info({?migrate_subscription_msg, FromSubscription, ToSubscription}, #state{id = Id, message_tab = MsgTab} = State) ->
    % Extract useful information from subscriptions
    #subscription{message_key = MessageKey, collapse_fields = OldCollapseFields} = FromSubscription,
    #subscription{min_log_level = NewMinLogLevel, collapse_fields = NewCollapseFields, throttle_sec = NewThrottle} = ToSubscription,

    % Loop thru cached messages
    ets:foldl(fun(CachedMessage, Acc) ->
        #cached_message{key = CMK, event = LogEvent, from = From, count = MsgCount, throttle_sec = Throttle} = CachedMessage,
        #log_entry{level = LogLevel} = LogEvent,
        {StoredMessageKey, _CollapseKey} = CMK,
        LogLevelBelow = data_protocol:log_level_to_integer(LogLevel) < data_protocol:log_level_to_integer(NewMinLogLevel),

        % What can be changed and what are expected actions?
        if
            % This cached message is not affected
            StoredMessageKey =/= MessageKey ->
                Acc;

            % min_log_level: send all accumulated messages if their log_level is less than new min_log_level (and delete them from ETS)
            % collapse_fields: send all accumulated messages if collapse_fields differs (and delete them from ETS)
            LogLevelBelow; OldCollapseFields =/= NewCollapseFields ->
                send_message(Id, LogEvent, MsgCount),
                ets:delete(MsgTab, CMK),
                Acc + 1;

            % throttle_sec: just update to and throttle_sec fields of cached message
            Throttle =/= NewThrottle ->
                ets:insert(MsgTab, CachedMessage#cached_message{to = From + NewThrottle, throttle_sec = NewThrottle}),
                Acc + 1;

            % No actions required
            true -> Acc
        end
    end, 0, MsgTab),
    {noreply, State};

handle_info({catalogue, _CardKey}, #state{kind = Kind, id = Id, display_name = DisplayName, real_name = RealName, subscription_tab = SubTab, message_tab = MsgTab} = State) ->
    rearm_subscriptions(SubTab, MsgTab, Kind, Id, DisplayName, RealName),
    {noreply, State};

handle_info({MsgKey, #log_entry{level = Level, msg_count = MsgCount} = LogEvent}, #state{id = ReceiverId, subscription_tab = SubTab, message_tab = MsgTab} = State) ->
    % Get subscription from ETS
    #subscription{min_log_level = MinLogLevel, throttle_sec = Throttle, collapse_fields = CollapseFields } = ets_subscription(SubTab, MsgKey),

    % Check if message log level is greater or equal to minimal log level
    CanProcess = data_protocol:log_level_to_integer(Level) >= data_protocol:log_level_to_integer(MinLogLevel),

    % Process message
    ?doif(CanProcess, case Throttle of
        N when N > 0 ->
            % Convert message to JSON and determine cached message key. Expected format: {MsgKey :: tuple(), CollapseKey :: map()}
            LogEventJson = web_protocol:log_entry_to_json(LogEvent),
            CollapseKey = lists:foldl(fun(Key, Acc) -> Acc#{Key => maps:get(Key, LogEventJson, undefined)} end, #{}, CollapseFields),
            CachedMsgKey = {MsgKey, CollapseKey},

            % Get cached message
            CachedMessage = ets_cached_message(MsgTab, CachedMsgKey),

            % Check if message should be emitted now or just cached
            Now = time:seconds(),
            {ShouldNotify, ActualCount, NewCachedMessage} = case CachedMessage of
                undefined ->
                    CM = #cached_message{key = CachedMsgKey, event = LogEvent, from = Now, to = Now + Throttle, count = 0, throttle_sec = Throttle},
                    {true, MsgCount, CM};
                #cached_message{to = To, count = Count} = M when To =< Now ->
                    CM = M#cached_message{event = LogEvent, from = Now, to = Now + Throttle, count = 0, throttle_sec = Throttle},
                    {true, Count + MsgCount, CM};
                #cached_message{count = Count} = M ->
                    CM = M#cached_message{event = LogEvent, count = Count + MsgCount},
                    {false, 0, CM}
            end,

            % Update entry in ETS
            ets:insert(MsgTab, NewCachedMessage),

            % Send if possible
            ?doif(ShouldNotify andalso ActualCount > 0, send_message(ReceiverId, LogEvent, ActualCount));
        _ ->
            % Send instantly
            send_message(ReceiverId, LogEvent, MsgCount)
    end),
    {noreply, State};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

ets_tab(Kind, Id, Suffix) ->
    list_to_atom(lists:flatten(io_lib:format("~s_~s_~s_~s", [?MODULE, Kind, Id, Suffix]))).

format_key({App, Component, Branch}) ->
    iolist_to_binary(io_lib:format("~s:~s:~s", [App, Component, Branch])).

components() ->
    AppKeys = card_config:applications(),
    Apps = [cards:card_application(Key) || Key <- AppKeys],
    Pairs = [{App, CompKeys} || #card_application{key = App, components = CompKeys} <- Apps],
    [{App, [cards:card_component(CK) || CK <- CompKeys]} || {App, CompKeys} <- Pairs].

is_valid_person(DisplayName, RealName, #slack_receiver{kind = person, title = Id}) ->
    DisplayName1 = slack_util:to_lower_trim(DisplayName),
    RealName1 = slack_util:to_lower_trim(RealName),
    Id1 = slack_util:to_lower_trim(Id),
    Id1 =:= DisplayName1 orelse Id1 =:= RealName1;
is_valid_person(_, _, _) ->
    false.

is_valid_channel(ChannelId, #slack_receiver{kind = channel, title = Id}) ->
    ChannelId1 = slack_util:to_lower_trim(ChannelId),
    Id1 = slack_util:to_lower_trim(Id),
    Id1 =:= ChannelId1;
is_valid_channel(_, _) ->
    false.

subscriptions(Kind, Id, DisplayName, RealName) ->
    ValidationFun = case Kind of
        person -> fun(R) -> is_valid_person(DisplayName, RealName, R) end;
        channel -> fun(R) -> is_valid_channel(Id, R) end
    end,
    SubList = [begin
        lists:filtermap(fun(#card_component{key = ComponentKey, template = TK, slack_receivers = SR}) ->
            CollapseFields = case cards:card_component_template(TK) of
                #card_component_template{collapse_fields = []} -> [<<"level">>, <<"message">>];
                #card_component_template{collapse_fields = CF} -> CF
            end,
            case [R || R <- SR, ValidationFun(R)] of
                [] -> false;
                VR ->
                    Result = [begin
                        MessageKey = {
                            slack_util:to_lower_trim(App),
                            slack_util:to_lower_trim(ComponentKey),
                            ?yesno(is_binary(Branch), slack_util:to_lower_trim(Branch), undefined)
                        },
                        #subscription{
                            message_key = MessageKey,
                            min_log_level = MinLogLevel,
                            throttle_sec = ThrottleSec,
                            collapse_fields = lists:usort(CollapseFields)
                        }
                    end || #slack_receiver{branch = Branch, min_log_level = MinLogLevel, throttle_sec = ThrottleSec} <- VR],
                    {true, Result}
            end
        end, Components)
    end || {App, Components} <- components()],
    lists:flatten(SubList).

ets_subscription(SubTab, Key) ->
    [Subscription | _] = ets:lookup(SubTab, Key),
    Subscription.

ets_cached_message(MsgTab, Key) ->
    case ets:lookup(MsgTab, Key) of
        [#cached_message{} = CachedMessage | _] -> CachedMessage;
        _ -> undefined
    end.

delete_cached_messages(MsgTab, MsgKey) ->
    ets:foldl(fun
        (#cached_message{key = {MK, CK}}, _Acc) when MK =:= MsgKey -> ets:delete(MsgTab, {MK, CK});
        (_, Acc) -> Acc
    end, [], MsgTab).

rearm_subscriptions(SubTab, MsgTab, Kind, Id, DisplayName, RealName) ->
    % Get actual subscriptions and their message keys
    Subscriptions = subscriptions(Kind, Id, DisplayName, RealName),
    NewKeys = ordsets:from_list([MK || #subscription{message_key = MK} <- Subscriptions]),
    SubscriptionMap = lists:foldl(fun(#subscription{message_key = MK} = S, Acc) -> Acc#{MK => S} end, #{}, Subscriptions),

    % Get message keys from existing subscriptions
    ExistingKeys = ets:foldl(fun(#subscription{message_key = MK}, AccSet) -> ordsets:add_element(MK, AccSet) end, ordsets:new(), SubTab),

    % Determine prefix and name for logs
    Prefix = ?yesno(Kind =:= person, <<"@">>, <<"#">>),
    Name = ?yesno(Kind =:= person, DisplayName, Id),

    % Determine subscriptions to delete
    DeleteTheseKeys = ordsets:subtract(ExistingKeys, NewKeys),
    [begin
        gproc:unreg({p, l, Key}),
        ets:delete(SubTab, Key),
        delete_cached_messages(MsgTab, Key),
        logger:debug("~s~s unsubscribed from <~s>", [Prefix, Name, format_key(Key)], #{caption => ?MODULE})
    end || Key <- DeleteTheseKeys],

    % Determine subscriptions to add
    AddTheseKeys = ordsets:subtract(NewKeys, ExistingKeys),
    [begin
        gproc:reg({p, l, Key}),
        ets:insert(SubTab, maps:get(Key, SubscriptionMap)),
        logger:debug("~s~s subscribed to <~s>", [Prefix, Name, format_key(Key)], #{caption => ?MODULE})
    end || Key <- AddTheseKeys],

    % Determine subscriptions to update
    UpdateTheseKeys = ordsets:intersection(ExistingKeys, NewKeys),
    [begin
        ExistingSubscription = ets_subscription(SubTab, Key),
        NewSubscription = maps:get(Key, SubscriptionMap),
        ?doif(ExistingSubscription =/= NewSubscription, begin
            ets:insert(SubTab, NewSubscription),
            erlang:send_after(0, self(), {?migrate_subscription_msg, ExistingSubscription, NewSubscription}),
            logger:debug("~s~s updated subscription to <~s>", [Prefix, Name, format_key(Key)], #{caption => ?MODULE})
        end)
    end || Key <- UpdateTheseKeys].

badge_color(trace) -> <<"#cccccc">>;
badge_color(debug) -> <<"#7cd197">>;
badge_color(info) -> <<"#34bfa3">>;
badge_color(notice) -> <<"#1d9bd1">>;
badge_color(warning) -> <<"#ffd700">>;
badge_color(error) -> <<"#e7828c">>;
badge_color(fatal) -> <<"#ff0000">>.

user_id(undefined) -> undefined;
user_id(Name) ->
    NameV2 = binary:replace(Name, <<".">>, <<" ">>, [global]),
    case slack_bot:user_id(Name) of
        undefined -> slack_bot:user_id(NameV2);
        UID -> UID
    end.

user_link(undefined) -> undefined;
user_link(Id) -> <<"slack://user?team=", (slack_config:team_id())/binary, "&id=", Id/binary>>.

fmt_version(<<"None">>) -> <<>>;
fmt_version(V) -> <<" (v. ", V/binary, ")">>.

send_message(ReceiverId, LogEvent, Count) ->
    proc_lib:spawn(fun() ->
        #log_entry{app = App, component = Component, branch = Branch, version = Version, level = Level, message = Message, data = MetaData} = LogEvent,
        {ok, RootUrl} = web_config:url(),
        Time = time:seconds(),
        QsVersions = ?yesno(Version =:= <<"None">>, <<>>, <<"?versions=", Version/binary>>),
        Title = <<App/binary, " | ", Component/binary, " | ", Branch/binary, " ", (fmt_version(Version))/binary>>,
        TitleLink = <<RootUrl/binary, "logs/", App/binary, "/", Component/binary, "/", Branch/binary, QsVersions/binary>>,
        Fallback = <<Title/binary, " - ", Message/binary>>,
        LevelStr = data_protocol:log_level_to_json(Level),
        % Pretext = ?yesno(Count > 1, <<LevelStr/binary, " (", (integer_to_binary(Count))/binary, ")">>, LevelStr),
        AuthorName = maps:get(<<"username">>, MetaData, undefined),
        AuthorId = user_id(AuthorName),
        LevelField = #attachment_field{title = <<"Level">>, value = LevelStr, short = false},
        RepeatedField = #attachment_field{title = <<"Repeated">>, value = integer_to_binary(Count), short = false},
        Fields = [LevelField] ++ ?yesno(Count > 1, [RepeatedField], []),
        Attachments = [
            #attachment_block{
                fallback = Fallback,
                color = badge_color(Level),
                % pretext = Pretext,
                author_name = AuthorName,
                author_link = user_link(AuthorId),
                title = Title,
                title_link = TitleLink,
                text = Message,
                fields = Fields,
                footer = <<"Chronos Slack Bot">>,
                footer_icon = <<"https://chronos.yourcompany.com/assets/app/media/img/logos/chronos/ChronosIcon32.png">>,
                ts = Time
            }
        ],
        try slack_api:post_message_ex(#post_message_ex_request{channel = ReceiverId, attachments = Attachments})
        catch _:_:_ -> ignore
        end
    end).
