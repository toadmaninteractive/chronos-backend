-module(util_unicode).

%% Exported functions

-export([
    to_unicode/1,           % Convert binary to a list of unicode characters
    strlen/1                % Returns size of unicode string
]).

%% API

-spec to_unicode(Binary :: binary() | string() | atom()) ->
    unicode:charlist().

to_unicode(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary, utf8);
to_unicode(String) when is_list(String) -> unicode:characters_to_list(String, utf8);
to_unicode(Atom) when is_atom(Atom) -> unicode:characters_to_list(Atom, utf8).

-spec strlen(Binary :: binary() | string()) ->
    non_neg_integer().

strlen(Binary) when is_binary(Binary) -> strlen(to_unicode(Binary));
strlen(Atom) when is_atom(Atom) -> strlen(to_unicode(Atom));
strlen(String) when is_list(String) -> length(String).

%% Local functions
