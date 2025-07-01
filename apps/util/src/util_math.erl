-module(util_math).

%% Exported functions

-export([
    to_float/1,             % Convert binary or integer to float
    float_equals/2,         % Compare if two floats are equal
    ceil/1                  % Ceil
]).

-define(FLOAT_ERROR, 0.000000001).

%% API

-spec to_float(Value :: float() | integer() | binary()) -> float().

to_float(Float) when is_float(Float) ->
    Float;
to_float(Int) when is_integer(Int) ->
    Int * 1.0;
to_float(Binary) when is_binary(Binary) ->
    try
        binary_to_float(Binary)
    catch _:_ ->
        binary_to_integer(Binary) * 1.0
    end.

-spec float_equals(A :: number(), B :: number()) -> boolean().

float_equals(A, B) ->
    abs(A * 1.0 - B * 1.0) < ?FLOAT_ERROR.

-spec ceil(X :: float()) ->
    integer().

ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Local functions
