-module(slack_util).

%% Exported functions

-export([
    to_lower_trim/1
]).

%% API

-spec to_lower_trim(binary()) -> binary().

to_lower_trim(Binary) ->
    Binary1 = binary:replace(Binary, [<<"@">>, <<"#">>], <<>>, [global]),
    Chars = string:lowercase(unicode:characters_to_list(string:trim(Binary1), utf8)),
    Binary2 = unicode:characters_to_binary(Chars),
    util_binary:trim(Binary2).

%% Local functions
