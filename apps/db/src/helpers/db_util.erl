-module(db_util).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("db.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    escape/1,
    maybe_null/1,
    result_to_json/2,
    db_fun_reply/1,
    get_patch/2,
    apply_modifier/2,
    mk_query_filter/4,
    mk_query_filter/5,
    mk_query_filter/6,
    mk_update/6,
    mk_update/8,
    mk_search/3,
    to_date/1,
    salt/0,
    salt/1,
    password_hash/2
]).

%% API

-spec escape(Text :: string() | binary()) -> binary().

escape(Binary) when is_binary(Binary) ->
    escape(util_unicode:to_unicode(Binary));
escape(Text) when is_list(Text) ->
    util_binary:to_binary([escape_char(Char) || Char <- Text]).

-spec maybe_null(Value :: 'undefined' | 'null' | any()) -> any().

maybe_null(undefined) -> null;
maybe_null(null) -> null;
maybe_null(Value) -> Value.

-spec result_to_json(Columns :: [#column{}], Rows :: [tuple()]) ->
    [jsx:json_term()].

result_to_json([], _) -> [];
result_to_json(Columns, Rows) ->
    [row_to_json(Columns, Row) || Row <- Rows].

-spec db_fun_reply(Result :: binary()) ->
    {'ok', EntityId :: non_neg_integer()} | {'error', Reason :: atom()}.

db_fun_reply(<<"ok">>) ->
    ok;
db_fun_reply(Result) when is_binary(Result) ->
    try
        {ok, binary_to_integer(Result)}
    catch
        _:_ -> {error, binary_to_atom(Result, latin1)}
    end.

-spec get_patch(Fields :: [atom() | binary()], Patch :: maps:map()) ->
    maps:map().

get_patch(Fields, Patch) ->
    lists:foldl(fun(Field, Acc) ->
        Field1 = util_binary:to_binary(Field),
        case maps:get(Field1, Patch, not_exists) of
            not_exists -> Acc;
            Value -> Acc#{Field1 => Value}
        end
    end, #{}, Fields).

-spec apply_modifier(Modifier :: {Param :: atom() | binary() | string(), ModType :: atom()}, Value :: binary() | 'undefined') ->
    binary().

apply_modifier({Param, ModType}, Value) ->
    Param1 = util_binary:to_binary(Param),
    Value1 = util_binary:to_binary(Value),
    case ModType of
        ?mod_inc -> <<Param1/binary, " + 1">>;
        ?mod_set_now -> <<"current_timestamp">>;
        ?mod_trim -> <<"TRIM(", Value1/binary, ")">>;
        ?mod_lower -> <<"LOWER(", Value1/binary, ")">>;
        ?mod_lower_trim -> <<"LOWER(TRIM(", Value1/binary, "))">>;
        ?mod_upper -> <<"UPPER(", Value1/binary, ")">>;
        ?mod_upper_trim -> <<"UPPER(TRIM(", Value1/binary, "))">>;
        ?mod_coalesce-> <<"COALESCE(", Param1/binary, ", ", Value1/binary, ")">>
    end.

-spec mk_query_filter(OrderBy :: binary(), OrderDir :: binary() | integer(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    binary().

mk_query_filter(OrderBy, OrderDir, Offset, Limit) ->
    mk_query_filter(OrderBy, <<>>, OrderDir, Offset, Limit).

-spec mk_query_filter(OrderBy :: binary(), OrderByPrefix :: binary(), OrderDir :: binary() | integer(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    binary().

mk_query_filter(OrderBy, OrderByPrefix, OrderDir, Offset, Limit) ->
    mk_query_filter(OrderBy, OrderByPrefix, OrderDir, Offset, Limit, undefined).

-spec mk_query_filter(OrderBy :: binary(), OrderByPrefix :: binary(), OrderDir :: binary() | integer(), Offset :: non_neg_integer(), Limit :: non_neg_integer(), Filter :: maps:map()) ->
    binary().

mk_query_filter(OrderBy, OrderByPrefix, OrderDir, Offset, Limit, _Filter) ->
    OrderDir1 = if
        is_integer(OrderDir), OrderDir < 0 -> <<"DESC">>;
        is_integer(OrderDir) -> <<"ASC">>;
        is_binary(OrderDir) ->
            OD = string:to_lower(unicode:characters_to_list(OrderDir, utf8)),
            case unicode:characters_to_binary(OD, utf8, utf8) of
                <<"asc">> -> <<"ASC">>;
                <<"desc">> -> <<"DESC">>;
                _ -> <<"ASC">>
            end
    end,
    OrderBy1 = ?yesno(is_binary(OrderByPrefix) andalso OrderByPrefix =/= <<>>, <<OrderByPrefix/binary, ".", OrderBy/binary>>, OrderBy),
    OrderNulls = ?yesno(OrderDir1 =:= <<"ASC">>, <<"NULLS FIRST">>, <<"NULLS LAST">>),
    Offset1 = util_binary:to_binary(Offset),
    Limit1 = util_binary:to_binary(Limit),
    <<
        " ORDER BY ", OrderBy1/binary, " ", OrderDir1/binary, " ", OrderNulls/binary,
        " OFFSET ", Offset1/binary,
        " LIMIT ", Limit1/binary
    >>.

-spec mk_update(Table, IdField, EntityId, Fields, AutoSetFields, Json) -> Result when
    Table :: atom() | binary() | string(),
    IdField :: atom() | binary() | string(),
    EntityId :: non_neg_integer() | atom() | binary() | string(),
    Fields :: [atom() | binary() | string() | {atom() | binary() | string(), Modifier :: atom()}],
    AutoSetFields :: [atom() | binary() | string()],
    Json :: maps:map(),
    Result :: {'ok', Query :: binary(), Params :: [jsx:json_term()]} | {'error', Reason :: atom()}.

mk_update(Table, IdField, EntityId, Fields, AutoSetFields, Json) ->
    mk_update(Table, IdField, EntityId, undefined, undefined, Fields, AutoSetFields, Json).

-spec mk_update(Table, IdField, EntityId, RevField, Rev, Fields, AutoSetFields, Json) -> Result when
    Table :: atom() | binary() | string(),
    IdField :: atom() | binary() | string(),
    EntityId :: non_neg_integer() | atom() | binary() | string(),
    RevField :: atom() | binary() | string() | 'undefined',
    Rev :: non_neg_integer() | 'undefined',
    Fields :: [atom() | binary() | string() | {atom() | binary() | string(), Modifier :: atom()}],
    AutoSetFields :: [atom() | binary() | string()],
    Json :: maps:map(),
    Result :: {'ok', Query :: binary(), Params :: [jsx:json_term()]} | {'error', Reason :: atom()}.

mk_update(Table, IdField, EntityId, RevField, Rev, Fields, AutoSetFields, Json) ->
    % Convert to binary when appropriate
    Table1 = util_binary:to_binary(Table),
    IdField1 = util_binary:to_binary(IdField),
    EntityId1 = ?yesno(is_integer(EntityId), EntityId, util_binary:to_binary(EntityId)),
    {FieldNames, FieldMap} = lists:foldr(fun(Field, {NameAcc, MapAcc}) ->
        {FieldName, Modifier} = case Field of
            {FN, Mod} -> {FN, Mod};
            FN -> {FN, undefined}
        end,
        FieldName1 = util_binary:to_binary(FieldName),
        {[FieldName1|NameAcc], MapAcc#{FieldName1 => Modifier}}
    end, {[], #{}}, Fields),

    % Generate patch
    case get_patch(FieldNames, Json) of
        Patch when is_map(Patch), map_size(Patch) =< 0 ->
            % No fields to update
            {error, nothing_to_update};
        Patch when is_map(Patch) ->
            % Check if revision is present
            HasRevision = is_binary(RevField) andalso RevField =/= <<>> andalso is_integer(Rev) andalso Rev > 0,
            RevPart = ?yesno(HasRevision, <<" AND ", (util_binary:to_binary(RevField))/binary, " = $2 ">>, <<>>),

            % We've fields to update
            StartIndex = ?yesno(HasRevision, 3, 2), % Preserve $1 for %entity_id% and optionally $2 for %rev%

            % Generate update pairs and params list
            {ReversedPairs, ReversedParams, _} = maps:fold(fun(K, V, {PairAcc, ParamAcc, Index}) ->
                % Alter value to be suitable for SQL query
                V1 = case V of
                    List when is_list(List) -> jsx:encode(List);
                    Map when is_map(Map) -> jsx:encode(Map);
                    Value -> Value
                end,

                % Convert index to binary
                I = util_binary:to_binary(Index),

                % Construct value with modifiers
                ValueWithMod = case maps:get(K, FieldMap, undefined) of
                    undefined -> <<"$", I/binary>>;
                    Mod -> apply_modifier({K, Mod}, <<"$", I/binary>>)
                end,

                % Make pair
                Pair = <<K/binary, " = ", ValueWithMod/binary>>,

                % Store pair and param
                {[Pair|PairAcc], [V1|ParamAcc], Index + 1}
            end, {[], [], StartIndex}, Patch),

            % Reverse pairs
            Pairs = lists:reverse(ReversedPairs),

            % Make pairs for auto-set fields
            AutoSetPairs = [begin
                FN = util_binary:to_binary(Field),
                VM = apply_modifier({Field, Mod}, undefined),
                <<FN/binary, " = ", VM/binary>>
            end || {Field, Mod} <- AutoSetFields],

            % Make set fields
            SetFields = util_binary:join(Pairs ++ AutoSetPairs, <<", ">>),

            Query = <<
                "UPDATE ", Table1/binary, " ",
                "SET ", SetFields/binary, " ",
                "WHERE ", IdField1/binary, " = $1 ",
                RevPart/binary
            >>,
            Params = [EntityId1] ++ ?yesno(HasRevision, [Rev], []) ++ lists:reverse(ReversedParams),
            {ok, Query, Params};
        _ ->
            % Something is not right here
            {error, badmap}
    end.

-spec mk_search(Conditions :: [any()], StartIndex :: non_neg_integer(), NonEmptyPrefix :: binary()) ->
    {Where :: binary(), Params :: [any()]}.

mk_search(Conditions, StartIndex, NonEmptyPrefix) ->
    Result = lists:foldl(fun
        (undefined, Acc) -> Acc;
        (Part, {C, P, I}) when is_binary(Part) ->
            {[Part|C], P, I};
        ({Part, V}, {C, P, I}) when is_list(V) ->
            Count = length(V),
            QI = [<<"$", (integer_to_binary(II))/binary>> || II <- lists:seq(I, I + Count - 1)],
            QIS = util_binary:join(QI, <<", ">>),
            RV = lists:reverse(V),
            Part1 = binary:replace(Part, <<"$">>, QIS, [global]),
            {[Part1|C], RV ++ P, I + Count};
        ({Part, V}, {C, P, I}) ->
            Part1 = binary:replace(Part, <<"$">>, <<"$", (integer_to_binary(I))/binary>>, [global]),
            {[Part1|C], [V|P], I + 1}
    end, {[], [], StartIndex}, Conditions),

    case Result of
        {[], _, _} ->
            {<<>>, []};
        {C, P, _} ->
            C1 = util_binary:join(lists:reverse(C), <<" AND ">>),
            {<<" ", NonEmptyPrefix/binary, " ", C1/binary, " ">>, lists:reverse(P)}
    end.

-spec to_date(Date :: calendar:datetime() | non_neg_integer() | 'undefined') ->
    calendar:datetime() | 'undefined'.

to_date({Date, Time}) ->
    {Date, Time};
to_date(UnixTimestamp) when is_integer(UnixTimestamp), UnixTimestamp >= 0 ->
    util_time:seconds_to_datetime(UnixTimestamp);
to_date(undefined) ->
    undefined.

-spec salt() -> binary().

salt() ->
    salt(64).

-spec salt(Length :: non_neg_integer()) -> binary().

salt(Length) when is_integer(Length), Length > 0 ->
    util_hex:from_binary(crypto:strong_rand_bytes(Length)).

-spec password_hash(Password :: binary(), Salt :: binary()) -> binary().

password_hash(Password, Salt) ->
    PasswordHash = crypto:hash(sha512, <<Salt/binary, Password/binary>>),
    util_hex:from_binary(PasswordHash).

%% Local functions

escape_char(Char) ->
    case lists:member(Char, "\\|*+?{}().,$^%#") of
        true -> <<$\\, Char>>;
        false -> <<Char>>
    end.

row_to_json([], _) -> [];
row_to_json(Columns, Row) -> maps:from_list(row_to_json(Columns, Row, 1)).

row_to_json([], _Row, _Index) -> [];
row_to_json([#column{name = Key, type = ColumnType}|Tail], Row, Index) ->
    Value = element(Index, Row),
    [{Key, column_to_json(ColumnType, Value)} | row_to_json(Tail, Row, Index + 1)].

column_to_json(_, null) -> null;
column_to_json(bool, Value) -> Value;
column_to_json(int2, Value) -> Value;
column_to_json(int4, Value) -> Value;
column_to_json(int8, Value) -> Value;
column_to_json(float2, Value) -> Value;
column_to_json(float4, Value) -> Value;
column_to_json(float8, Value) -> Value;
column_to_json(numeric, Value) when is_binary(Value) -> binary_to_float(Value);
column_to_json(timestamp, DateTime) -> util_time:iso_8601_fmt_tz(DateTime, utc);
column_to_json(timestamptz, DateTime) -> util_time:iso_8601_fmt_tz(DateTime, utc);
column_to_json({array, ColumnType}, List) when is_list(List) -> [column_to_json(ColumnType, Value) || Value <- List];
column_to_json(varchar, Value) when is_binary(Value) -> Value;
column_to_json(jsonb, Value) when is_binary(Value) -> jsx:decode(Value, [return_maps]);
column_to_json({unknown_oid, 1700}, Value) -> column_to_json(numeric, Value);   % fix for numeric
column_to_json(_T, Value) when is_binary(Value) -> Value;
column_to_json(_, Value) when is_integer(Value) -> integer_to_binary(Value).
