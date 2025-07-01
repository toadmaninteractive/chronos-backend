% Headers
-define(x_session_id, <<"x-session-id">>).
-define(x_session_id_cs, <<"X-Session-Id">>).

% Cookie
-define(x_cookie_sid, <<"sid">>).

% Meta
-define(m_session, '$RestApiSession').

-record(session, {
    key :: {'personnel', binary()},
    user_id :: non_neg_integer(),
    created_at :: non_neg_integer(),
    valid_thru :: non_neg_integer()
}).

-type session() :: #session{}.