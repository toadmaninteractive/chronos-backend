-module(db_if_personnel).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_username/1,
    get/5,
    get_count/1,
    get_all/0,
    create/4,
    update/2,
    block/1,
    unblock/1,
    delete/1,
    undelete/1,
    exists/1,
    credentials/1
]).

%% API

-spec get_one(PersonnelId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(PersonnelId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel AS per ",
        (common_joins())/binary,
        "WHERE per.id = $1 "
    >>,
    case db_query:select_one(Query, [PersonnelId]) of
        {ok, Account} -> {ok, Account};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_username(Username :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_username(Username) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel AS per ",
        (common_joins())/binary,
        "WHERE per.username = $1 "
    >>,
    case db_query:select_one(Query, [Username]) of
        {ok, Account} -> {ok, Account};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(), Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"WHERE strpos(LOWER(per.username), LOWER($1)) > 0 OR strpos(LOWER(per.name), LOWER($1)) > 0">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel AS per ",
        (common_joins())/binary,
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_count(Needle) -> Result when
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"WHERE strpos(LOWER(per.username), LOWER($1)) > 0 OR strpos(LOWER(per.name), LOWER($1)) > 0">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM personnel AS per ",
        NeedlePart/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all() ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel AS per ",
        (common_joins())/binary,
        "ORDER BY per.id ASC"
    >>,
    db_query:select(Statement, []).

-spec create(Username, Name, Email, Phone) -> Result when
    Username :: binary(),
    Name :: binary() | 'null',
    Email :: binary() | 'null',
    Phone :: binary() | 'null',
    Result :: {'ok', PersonnelId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(Username, Name, Email, Phone) ->
    Params = [Username , Name, Email, Phone],
    Query = <<
        "INSERT INTO personnel (username, name, email, phone) ",
        "VALUES (LOWER(TRIM($1)), TRIM($2), LOWER(TRIM($3)), LOWER(TRIM($4))) RETURNING id::bigint"
    >>,
    case db_query:insert(Query, Params) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := PersonnelId}] = db_util:result_to_json(Columns, Rows),
            {ok, PersonnelId};
        {error, unique_violation} ->
            {error, ?err_already_exists};
        {error, Other} ->
            {error, Other}
    end.

-spec update(PersonnelId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(PersonnelId, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_lower_trim(username),
        ?mk_mod_trim(name),
        ?mk_mod_lower_trim(email),
        ?mk_mod_lower_trim(phone),
        is_blocked,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"personnel">>, <<"id">>, PersonnelId, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec block(PersonnelId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

block(PersonnelId) ->
    update(PersonnelId, #{<<"is_blocked">> => true}).

-spec unblock(PersonnelId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

unblock(PersonnelId) ->
    update(PersonnelId, #{<<"is_blocked">> => false}).

-spec delete(PersonnelId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(PersonnelId) ->
    update(PersonnelId, #{<<"is_deleted">> => true}).

-spec undelete(PersonnelId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(PersonnelId) ->
    update(PersonnelId, #{<<"is_deleted">> => false}).

-spec exists(Username :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Username) ->
    Query = <<"SELECT COUNT(id)::integer AS count FROM personnel WHERE username = LOWER(TRIM($1))">>,
    case db_query:select_one(Query, [Username]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec credentials(Username :: binary()) ->
    {'ok', UserId :: non_neg_integer(), Email :: binary() | 'null', IsBlocked :: boolean(), IsDeleted :: boolean()}
    | {'error', Reason :: atom()}.

credentials(Username) ->
    Query = <<
        "SELECT ", (credential_fields())/binary,
        "FROM personnel AS per ",
        "WHERE LOWER(TRIM(per.username)) = LOWER(TRIM($1))"
    >>,
    case db_query:select_one(Query, [Username]) of
        {ok, #{<<"id">> := UserId, <<"email">> := Email, <<"is_blocked">> := IsBlocked, <<"is_deleted">> := IsDeleted}} ->
            {ok, UserId, Email, IsBlocked, IsDeleted};
        {error, Reason} ->
            {error, Reason};
        undefined ->
            {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % per : personnel
    <<
        "per.id AS id, ",
        "per.rev AS rev, ",
        "per.username AS username, ",
        "per.name AS name, ",
        "per.email AS email, ",
        "per.phone AS phone, ",
        "per.is_blocked AS is_blocked, ",
        "per.is_deleted AS is_deleted, ",
        "per.created_at AS created_at, ",
        "per.updated_at AS updated_at "
    >>.

credential_fields() ->
    % Aliases:
    % per : personnel
    <<
        "per.id AS id, ",
        "per.email AS email, ",
        "per.is_blocked AS is_blocked, "
        "per.is_deleted AS is_deleted "
    >>.

common_joins() ->
    <<>>.
