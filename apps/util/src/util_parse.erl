-module(util_parse).

%% Exported functions

-export([
    maybe_int/1,            % Parse integer number
    maybe_float/1           % Compare if two floats are equal
]).

%% API

-spec maybe_int(Value :: integer() | binary()) -> integer().

maybe_int(Int) when is_integer(Int) ->
    Int;
maybe_int(Binary) when is_binary(Binary) ->
    binary_to_integer(Binary).

-spec maybe_float(Value :: float() | integer() | binary()) -> float().

maybe_float(Float) when is_float(Float) ->
    Float;
maybe_float(Int) when is_integer(Int) ->
    Int * 1.0;
maybe_float(Binary) when is_binary(Binary) ->
    try
        maybe_float(maybe_int(Binary))
    catch _:_ ->
        binary_to_float(Binary)
    end.

%% Local functions
