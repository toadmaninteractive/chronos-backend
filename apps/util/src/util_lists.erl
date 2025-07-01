-module(util_lists).

%% Exported functions

-export([
    to_list/1,              % Convert to list (accepts integer | atom | binary | list)
    to_chunks/2,            % Split list into chunks
    split/2                 % Safe list split
]).

%% API

-spec to_list(Value :: integer() | atom() | binary() | list()) -> list().

to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Float) when is_float(Float) -> float_to_list(Float, [{decimals, 8}, compact]);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(List) when is_list(List) -> List.

-spec to_chunks(List :: list(), ChunkSize :: non_neg_integer()) ->
    [list()].

to_chunks(List, ChunkSize) ->
    ReversedChunks = to_chunks_reverse(List, [], 0, ChunkSize),
    [lists:reverse(Chunk) || Chunk <- lists:reverse(ReversedChunks)].

-spec split(Size :: non_neg_integer(), List :: list()) -> list().

split(Size, List) when Size >= 0 ->
    split(Size, List, []).

%% Local functions

to_chunks_reverse([], Acc, _, _) ->
    Acc;
to_chunks_reverse([H|T], Acc, Pos, ChunkSize) when Pos =:= ChunkSize ->
    to_chunks_reverse(T, [[H] | Acc], 1, ChunkSize);
to_chunks_reverse([H|T], [HAcc | TAcc], Pos, ChunkSize) ->
    to_chunks_reverse(T, [[H | HAcc] | TAcc], Pos + 1, ChunkSize);
to_chunks_reverse([H|T], [], Pos, ChunkSize) ->
    to_chunks_reverse(T, [[H]], Pos + 1, ChunkSize).

split(0, L, R) -> {lists:reverse(R), L};
split(N, [H | T], R) -> split(N - 1, T, [H | R]);
split(_, [], R) -> {lists:reverse(R), []}.
