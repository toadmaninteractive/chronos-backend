-module(web_util).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    encode_json/1,
    decode_json/1,
    decode_json_safe/2,
    maybe_null/1,
    uuid32/0,
    uuid64/0,
    collect_body/1,
    match_path/2,
    enquote_sql/1,
    sanitize_sql/1,
    trim_list/2
]).

-define(max_body_length, 128 * 1024).   % 128 KB

%% API

-spec encode_json(Json :: jsx:json_term()) -> jsx:json_text().

encode_json(Json) ->
    jsx:encode(Json).

-spec decode_json(Binary :: jsx:json_text()) -> jsx:json_term().

decode_json(Binary) ->
    jsx:decode(Binary, [return_maps]).

-spec decode_json_safe(Binary :: jsx:json_text(), Default :: jsx:json_term()) -> jsx:json_term().

decode_json_safe(Binary, Default) ->
    try jsx:decode(Binary, [return_maps])
    catch _:_ -> Default
    end.

-spec maybe_null(Value :: 'null' | jsx:json_term()) -> jsx:json_term().

maybe_null(null) -> undefined;
maybe_null(Value) -> Value.

-spec uuid32() -> binary().

uuid32() ->
    util_hex:from_binary(uuid:get_v4(strong)).

-spec uuid64() -> binary().

uuid64() ->
    Uuid1 = util_hex:from_binary(uuid:get_v4(strong)),
    Uuid2 = util_hex:from_binary(uuid:get_v4(strong)),
    <<Uuid1/binary, Uuid2/binary>>.

-spec collect_body(cowboy_req:req()) ->
    {'ok', cowboy_req:req()}.

collect_body(Req) ->
    collect_body(Req, []).

-spec match_path(Path :: binary(), Regex :: [binary()]) ->
    {'ok', Prefix :: binary()} | 'nomatch'.

match_path(Path, Regex) ->
    lists:foldl(fun
        (RE, nomatch) ->
            case re:run(Path, RE) of
                {match, _} -> {ok, RE};
                _ -> nomatch
            end;
        (_, Acc) ->
            Acc
    end, nomatch, Regex).

-spec enquote_sql(Text :: binary()) -> binary().

enquote_sql(Text) ->
    Text1 = re:replace(Text, "\"", "\\\\\"", [{return, binary}, global]),
    re:replace(Text1, "\n", "\\\\n", [{return, binary}, global]).

-spec sanitize_sql(Text :: binary()) -> binary().

sanitize_sql(Text) ->
    re:replace(Text, "['\r\n]+", "", [{return, binary}, global]).

-spec trim_list(Arg, TrimFn) -> Result when
    Arg :: [binary()] | 'undefined',
    TrimFn :: fun((Elem :: binary()) -> binary()),
    Result :: [binary()] | 'undefined'.

trim_list(undefined, _) ->
    undefined;

trim_list(List, TrimFn) when is_list(List) ->
    L = [TrimFn(E) || E <- List],
    [E || E <- L, E =/= <<>>].

%% Local functions

collect_body(Req, Acc) ->
    % Read whole body
    case cowboy_req:read_body(Req) of
        {ok, Data, Req2} -> {ok, erlang:iolist_to_binary(lists:reverse([Data | Acc])), Req2};
        {more, Data, Req2} -> collect_body(Req2, [Data | Acc])
    end.
