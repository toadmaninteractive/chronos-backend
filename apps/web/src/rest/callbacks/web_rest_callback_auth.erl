-module(web_rest_callback_auth).

%% Include files
-include_lib("aplib/include/apmacros.hrl").

-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("common_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    login/2,
    logout/1,
    get_my_profile/1
]).

%% API

-spec login(LoginRequest, Req) -> Response when
    LoginRequest :: web_protocol:login_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:account(), cowboy_req:req()}.

login(_, #{?m_session := #session{}} = _) ->
    web_rest_auth_login:login_403(#forbidden_error_ex{reason = already_logged_in});

login(#login_request{username = ReqUsername, password = ReqPassword}, Req) ->
    try
        LdapAuthResult = cerberus_authenticate(ReqUsername, ReqPassword),
        LocalCredentials = db_if_personnel:credentials(ReqUsername),
        ?doif(LdapAuthResult =/= ok, begin
            {Type, What} = LdapAuthResult,
            logger:debug("Personnel authentication ~p (username: ~s, reason: ~p)~n", [Type, ReqUsername, What], #{caption => ?MODULE})
        end),
        LoginResult = case {LdapAuthResult, LocalCredentials} of
            % Account exists in LDAP but absent in local DB: create new personnel account
            {ok, {error, ?err_not_exists}} ->
                {ok, PersonnelId} = db_if_personnel:create(ReqUsername, ReqUsername, ?null, ?null),
                {ok, PersonnelId, ?null};

            % Account exists both in LDAP and in local DB, but is blocked
            {ok, {ok, _, _, true = _IsBlocked, _}} ->
                {error, account_is_blocked};

            % Account exists both in LDAP and in local DB, but is deleted: undelete account
            {ok, {ok, PersonnelId, PersonnelEmail, _, true = _IsDeleted}} ->
                db_if_personnel:undelete(PersonnelId),
                {ok, PersonnelId, PersonnelEmail};

            % Account exists both in LDAP and in local DB
            {ok, {ok, PersonnelId, PersonnelEmail, _, _}} ->
                {ok, PersonnelId, PersonnelEmail};

            % Account does not exist in LDAP, but exists in local DB: delete account and all of its sessions
            {{reject, unknown_user}, {ok, PersonnelId, _, _, _}} ->
                web_session:delete_for(?actor_personnel, PersonnelId),
                db_if_personnel:delete(PersonnelId),
                {error, account_is_deleted};

            % Account does not exist both in LDAP and in local DB
            {{reject, unknown_user}, _} ->
                {error, account_not_exists};

            % Invalid username / password pair
            {{reject, invalidCredentials}, _} ->
                {error, invalid_password}
        end,

        case LoginResult of
            {ok, UserId, _Email} ->
                {ok, SessionId} = web_session:create(?actor_personnel, UserId),
                {ok, EncodedSession} = web_session:encode(?actor_personnel, SessionId),
                Req1 = cowboy_req:set_resp_header(?x_session_id_cs, EncodedSession, Req),
                {ok, SessionDuration} = db_if_settings:personnel_session_duration(),
                Req2 = cowboy_req:set_resp_cookie(?x_cookie_sid, EncodedSession, Req1, #{path => <<"/">>, max_age => SessionDuration}),
                {ok, Account} = db_if_personnel:get_one(UserId),
                {web_protocol:account_from_json(Account), Req2};
            {error, Reason} ->
                web_rest_auth_login:login_403(#forbidden_error_ex{reason = Reason})
        end
    catch
        _C:_R:_S ->
            web_rest_auth_login:login_500(#internal_server_error{message = failure})
    end.

-spec logout(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {data_protocol:generic_response(), cowboy_req:req()}.

logout(#{?m_session := #session{key = {?actor_personnel, SessionId}}} = Req) ->
    web_session:delete(?actor_personnel, SessionId),
    Req1 = cowboy_req:set_resp_cookie(?x_cookie_sid, <<>>, Req, #{path => <<"/">>, max_age => 0}),
    {#generic_response{result = true}, Req1};
logout(_) ->
    web_rest_auth_logout:logout_403(#forbidden_error_ex{reason = not_logged_in}).

-spec get_my_profile(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:account(), cowboy_req:req()}.

get_my_profile(#{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Account} = db_if_personnel:get_one(UserId),
    {web_protocol:account_from_json(Account), Req};
get_my_profile(_) ->
    web_rest_auth_logout:logout_403(#forbidden_error_ex{reason = not_logged_in}).

%% Local functions

cerberus_authenticate(Username, Password) ->
    {ok, AuthRealm} = web_config:auth_realm(),
    LowercaseUsername = util_binary:to_lower(Username),
    try
        case cerberus:authenticate(AuthRealm, LowercaseUsername, Password) of
            ok -> ok;
            {reject, Reason} -> {reject, Reason};
            {error, Reason} -> {error, Reason}
        end
    catch Type:What:StackTrace ->
        logger:error("Cerberus authentication failed (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
        {error, exception}
    end.
