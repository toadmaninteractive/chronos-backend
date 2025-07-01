-module(slack).

%% Include files

-include("slack_protocol.hrl").

%% Exported functions

-export([
    post_snippet/4,
    post_message/2,
    get_users/0
]).

%% API

-spec post_message(Channel, Text) -> Result when
    Channel :: binary(),
    Text :: binary(),
    Result :: 'ok'| {'error', binary()}.

post_message(Channel, Text) ->
    RequestContent = #post_message_request{channel = Channel, text = Text},
    Response = slack_api:post_message(RequestContent),
    case Response#post_message_response.error of
        undefined -> ok;
        _ -> {error, Response#post_message_response.error}
    end.

-spec post_snippet(Channel, Title, Content, InitialComment) -> Result when
    Channel :: binary(),
    Title :: binary(),
    Content :: binary(),
    InitialComment :: binary(),
    Result :: boolean().

post_snippet(Channel, Title, Content, InitialComment) ->
    RequestContent = #post_snippet_request{channels = Channel, content = Content, initial_comment = InitialComment, title = Title},
    case slack_api:post_snippet(RequestContent) of
        #post_snippet_response{ok = Result} -> Result;
        _ -> false
    end.

-spec get_users() -> Result when
    Result :: [slack_protocol:member()] | 'undefined'.

get_users() ->
    case slack_api:get_users() of
        #get_users_response{members = Members} -> Members;
        _ -> undefined
    end.

%% Local functions
