-module(util_hex).

%% Exported functions

-export([
    from_binary/1,          % Convert binary to hexadecimal binary string
    to_binary/1             % Convert hexadecimal binary string to binary
]).

%% API

-spec from_binary(Binary :: binary()) ->
    binary().

from_binary(Binary) when is_binary(Binary) ->
    << <<(halfbyte_to_hex(H)), (halfbyte_to_hex(L))>> || <<H:4, L:4>> <= Binary >>.

-spec to_binary(Hex :: binary()) ->
    binary().

to_binary(Hex) when is_binary(Hex), byte_size(Hex) rem 2 =:= 0 ->
    << <<((hex_to_halfbyte(H) bsl 4) bor (hex_to_halfbyte(L) band 15))>> || <<H:8, L:8>> <= Hex >>;
to_binary(Hex) when is_binary(Hex), byte_size(Hex) rem 2 =:= 1 ->
    to_binary(<<"0", Hex/binary>>).

%% Local functions

halfbyte_to_hex(HalfByte) when HalfByte < 10 -> $0 + HalfByte;
halfbyte_to_hex(HalfByte) -> $a + HalfByte - 10.

hex_to_halfbyte(Hex) when Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_halfbyte(Hex) when Hex >= $A, Hex =< $F -> Hex - $A + 10;
hex_to_halfbyte(Hex) when Hex >= $a, Hex =< $f -> Hex - $a + 10.
