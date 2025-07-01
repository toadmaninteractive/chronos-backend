-module(util_validate).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    non_empty_string/1,     % Non-empty string (trim spaces both from left and right)
    email/1,                % E-mail
    username/1,             % Username
    jira_key/1,             % JIRA key
    selene_key/1,           % Selene project key
    url/1                   % URL
]).

%% API

-spec non_empty_string(Value :: binary()) ->
    boolean().

non_empty_string(Value) ->
    util_binary:pattern_match(Value, <<"^(?!\\s*$).+">>).

-spec email(Email :: binary()) ->
    boolean().

email(Email) ->
    util_binary:pattern_match(Email, <<"^[A-Za-z0-9-_]+(.[A-Za-z0-9-_]+)*@[A-Za-z0-9-]+(.[A-Za-z0-9-]+)*(.[A-Za-z]{2,4})$">>).

-spec username(Username :: binary()) ->
    boolean().

username(Username) ->
    util_binary:pattern_match(Username, <<"^(?=.{4,32}$)[A-Za-z]+([A-Za-z0-9]|([._-])(?![._-]))+(?<![._-])$">>).

-spec jira_key(JiraKey :: binary()) ->
    boolean().

jira_key(JiraKey) ->
    util_binary:pattern_match(JiraKey, <<"^[A-Z][A-Z0-9]+$">>).

-spec selene_key(SeleneKey :: binary()) ->
    boolean().

selene_key(SeleneKey) ->
    util_binary:pattern_match(SeleneKey, <<"^\\{[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}\\}$">>).

-spec url(Url :: binary()) ->
    boolean().

url(Url) ->
    case http_uri:parse(Url) of
        {ok, _} -> true;
        _ -> false
    end.

%% Local functions
