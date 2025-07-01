-module(util_binary).

%% Exported functions

-export([
    to_binary/1,            % Convert to binary (accepts binary | string | atom | integer | float)
    to_upper/1,             % Convert binary string to upper case
    to_lower/1,             % Convert binary string to lower case
    join/2,                 % Concatenate list of binaries with a separator
    pattern_match/2,        % Test if binary matches regex pattern
    trim/1,                 % Trim binary both from left and right
    ltrim/1,                % Trim binary both from left
    rtrim/1                 % Trim binary both from right
]).

%% API

-spec to_binary(binary() | string() | integer() | float() | atom()) ->
    binary().

to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(String) when is_list(String) -> unicode:characters_to_binary(String);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Float) when is_float(Float) -> float_to_binary(Float, [{decimals, 8}, compact]).

-spec to_upper(binary() | string()) ->
    binary().

to_upper(Binary) when is_binary(Binary) -> to_upper(binary_to_list(Binary));
to_upper(String) when is_list(String) -> iolist_to_binary(string:to_upper(String)).

-spec to_lower(binary() | string()) ->
    binary().

to_lower(Binary) when is_binary(Binary) -> to_lower(binary_to_list(Binary));
to_lower(String) when is_list(String) -> iolist_to_binary(string:to_lower(String)).

-spec join(ListOfBinaries :: [binary()], Separator :: binary()) -> binary().

join(ListOfBinaries, Separator) ->
    join(ListOfBinaries, Separator, <<>>).

-spec pattern_match(Subject :: binary(), Pattern :: re:mp() | binary()) -> boolean().

pattern_match(Subject, Pattern) ->
    case re:run(Subject, Pattern) of
        {match, _} -> true;
        _ -> false
    end.

-spec trim(Binary :: binary()) -> binary().

trim(Binary) ->
    trim(Binary, 32).

-spec ltrim(Binary :: binary()) -> binary().

ltrim(Binary) ->
    ltrim(Binary, 32).

-spec rtrim(Binary :: binary()) -> binary().

rtrim(Binary) ->
    rtrim(Binary, 32).

%% Local functions

join([], _, Acc) -> Acc;
join([H|T], Separator, Acc) when Acc =:= <<>> -> join(T, Separator, <<H/binary>>);
join([H|T], Separator, Acc) -> join(T, Separator, <<Acc/binary, Separator/binary, H/binary>>).

rtrim(B, X) when is_binary(B), is_integer(X) ->
    S = byte_size(B),
    do_rtrim(S, B, X);
rtrim(B, [_|_] = Xs) when is_binary(B) ->
    S = byte_size(B),
    do_mrtrim(S, B, Xs).

ltrim(B, X) when is_binary(B), is_integer(X) ->
    do_ltrim(B, X);
ltrim(B, [_|_] = Xs) when is_binary(B) ->
    do_mltrim(B, Xs).

trim(B, X) when is_binary(B), is_integer(X) ->
    From = ltrimc(B, X, 0),
    case byte_size(B) of
        From ->
            <<>>;
        S ->
            To = do_rtrimc(S, B, X),
            binary:part(B, From, To - From)
    end;
trim(B, [_|_] = Xs) when is_binary(B) ->
    From = mltrimc(B, Xs, 0),
    case byte_size(B) of
        From ->
            <<>>;
        S ->
            To = do_mrtrimc(S, B, Xs),
            binary:part(B, From, To - From)
    end.

do_ltrim(<<X, B/binary>>, X) ->
    do_ltrim(B, X);
do_ltrim(B, _X) ->
    B.

%% Multi, left trimming

do_mltrim(<<X, B/binary>> = XB, Xs) ->
    case ordsets:is_element(X, Xs) of
        true  -> do_mltrim(B, Xs);
        false -> XB
    end;
do_mltrim(<<>>, _Xs) ->
    <<>>.

do_rtrim(0, _B, _X) ->
    <<>>;
do_rtrim(S, B, X) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        X -> do_rtrim(S2, B, X);
        _ -> binary_part(B, 0, S)
    end.

%% Multiple version of do_rtrim

do_mrtrim(0, _B, _Xs) ->
    <<>>;
do_mrtrim(S, B, Xs) ->
    S2 = S - 1,
    X = binary:at(B, S2),
    case ordsets:is_element(X, Xs) of
        true  -> do_mrtrim(S2, B, Xs);
        false -> binary_part(B, 0, S)
    end.

ltrimc(<<X, B/binary>>, X, C) ->
    ltrimc(B, X, C + 1);
ltrimc(_B, _X, C) ->
    C.

%% Multi, left trimming, returns a count of matched bytes from the left

mltrimc(<<X, B/binary>>, Xs, C) ->
    case ordsets:is_element(X, Xs) of
        true  -> mltrimc(B, Xs, C+1);
        false -> C
    end;
mltrimc(<<>>, _Xs, C) ->
    C.

do_rtrimc(S, B, X) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        X -> do_rtrimc(S2, B, X);
        _ -> S
    end.

do_mrtrimc(S, B, Xs) ->
    S2 = S - 1,
    X = binary:at(B, S2),
    case ordsets:is_element(X, Xs) of
        true  -> do_mrtrimc(S2, B, Xs);
        false -> S
    end.
