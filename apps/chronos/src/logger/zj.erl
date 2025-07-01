%%% @doc
%%% ZJ: The tiny JSON parser
%%%
%%% This module exports four functions and accepts no options.
%%% @end

% Copyright 2019 Craig Everett <zxq9@zxq9.com>

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

-module(zj).
-vsn("1.0.5").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-export([encode/1]).

-type value()     :: string()
                   | number()
                   | true
                   | false
                   | undefined
                   | [value()]
                   | #{string() := value()}.

%%% Interface Functions

-spec encode(term()) -> string().

%% @doc
%% Take any convertable Erlang term and convert it to a JSON string.
%%
%% As JSON can only satirically be referred to as "a serialization format", it is
%% almost impossible to map any interesting data between Erlang (or any other language)
%% and JSON. For example, tuples do not exist in JSON, so converting an Erlang tuple
%% turns it into a list (a JSON array). Atoms also do not exist, so atoms other than
%% the ternay logic values `true', `false' and `null' become strings (those three
%% remain as atoms, with the added detail that JSON `null' maps to Erlang
%% `undefined').
%%
%% Unless care is taken to pick types that JSON can accurately express (integers,
%% floats, strings, maps, lists, ternary logic atoms) it is not possible to guarantee
%% (or even reasonable to expect) that `Term == decode(encode(Term))' will be true.
%%
%% This function crashes when it fails. Things that will cause a crash are trying to
%% convert non-UTF-8 binaries to strings, use non-string values as object keys,
%% encode an unaligned bitstring, etc.
%%
%% Note that Erlang terms are converted as type primitives, meaning that compound
%% functional structures like GB-trees, dicts, sets, etc. will wind up having their
%% underlying structures converted as-is which is almost never what you want. It is
%% usually best to reduce compound values down to primitives (lists or maps) before
%% running encode.
%%
%% The only unsupported Erlang pritmitive is bitstrings. Care has NOT been taken to
%% ensure separation between actual binary data and binaries that are supposed to be
%% interpreted as strings. The same is true of deep list data: it just comes out raw
%% unless you flatten or convert it to a utf8 string with the unicode module.
%%
%% NOTE: If you need a serialization format that is less ambiguous and expresses more
%% types consider using BERT (language-independent implementations of Erlang external
%% binary format) instead: http://bert-rpc.org

encode(true)                 -> "true";
encode(false)                -> "false";
encode(undefined)            -> "null";
encode([])                   -> "[]";
encode(T) when is_atom(T)    -> quote(atom_to_list(T));
encode(T) when is_float(T)   -> float_to_list(T);
encode(T) when is_integer(T) -> integer_to_list(T);
encode(T)                    -> unicode:characters_to_list(encode_value(T)).

%%% Encoding Functions

encode_value(true)                 -> "true";
encode_value(false)                -> "false";
encode_value(undefined)            -> "null";
encode_value(T) when is_atom(T)    -> quote(atom_to_list(T));
encode_value(T) when is_float(T)   -> float_to_list(T);
encode_value(T) when is_integer(T) -> integer_to_list(T);
encode_value(T) when is_binary(T)  -> maybe_string(T);
encode_value(T) when is_list(T)    -> maybe_array(T);
encode_value(T) when is_map(T)     -> pack_object(T);
encode_value(T) when is_tuple(T)   -> pack_array(tuple_to_list(T));
encode_value(T) when is_pid(T)     -> encode_value(pid_to_list(T));
encode_value(T) when is_port(T)    -> encode_value(port_to_list(T));
encode_value(T) when is_function(T) -> encode_value(erlang:fun_to_list(T));
encode_value(T) when is_reference(T) -> encode_value(ref_to_list(T)).

maybe_string(T) ->
    L = binary_to_list(T),
    true = io_lib:printable_unicode_list(L),
    quote(L).

maybe_array(T) ->
    case io_lib:printable_unicode_list(T) of
        true  -> quote(T);
        false -> pack_array(T)
    end.

quote(T) -> [$" | escape(T)].

escape([])        -> [$"];
escape([$\b | T]) -> [$\\, $b  | escape(T)];
escape([$\f | T]) -> [$\\, $f  | escape(T)];
escape([$\n | T]) -> [$\\, $n  | escape(T)];
escape([$\r | T]) -> [$\\, $r  | escape(T)];
escape([$\t | T]) -> [$\\, $t  | escape(T)];
escape([$\" | T]) -> [$\\, $"  | escape(T)];
escape([$\\ | T]) -> [$\\, $\\ | escape(T)];
escape([H | T])   -> [H | escape(T)].

pack_array([])        -> "[]";
pack_array([H | []])  -> [$[, encode_value(H), $]];
pack_array([H | T])   -> [$[, encode_value(H), $,, encode_array(T), $]].

encode_array([H | []]) -> encode_value(H);
encode_array([H | T])  -> [encode_value(H), $,, encode_array(T)].

pack_object(M) ->
    case maps:to_list(M) of
        [] ->
            "{}";
        [{K, V} | T] when is_list(K) ->
            true = io_lib:printable_unicode_list(K),
            Init = [$", K, $", $:, encode_value(V)],
            [${, lists:foldl(fun pack_object/2, Init, T), $}];
        [{K, V} | T] when is_binary(K) ->
            Key = unicode:characters_to_list(K),
            true = io_lib:printable_unicode_list(Key),
            Init = [$", Key, $", $:, encode_value(V)],
            [${, lists:foldl(fun pack_object/2, Init, T), $}];
        [{K, V} | T] when is_atom(K) ->
            Init = [$", atom_to_list(K), $", $:, encode_value(V)],
            [${, lists:foldl(fun pack_object/2, Init, T), $}]
    end.

pack_object({K, V}, L) when is_list(K) ->
    true = io_lib:printable_unicode_list(K),
    [$", K, $", $:, encode_value(V), $, | L];
pack_object({K, V}, L) when is_binary(K) ->
    Key = unicode:characters_to_list(K),
    true = io_lib:printable_unicode_list(Key),
    [$", Key, $", $:, encode_value(V), $, | L];
pack_object({K, V}, L) when is_float(K) ->
    Key = float_to_list(K),
    [$", Key, $", $:, encode_value(V), $, | L];
pack_object({K, V}, L) when is_integer(K) ->
    Key = integer_to_list(K),
    [$", Key, $", $:, encode_value(V), $, | L];
pack_object({K, V}, L) when is_atom(K) ->
    [$", atom_to_list(K), $", $:, encode_value(V), $, | L].
