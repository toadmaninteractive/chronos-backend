-module(web_rest_callback_ingest_ndjson).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("data_protocol.hrl").
-include("ingestion_protocol.hrl").

%% Exported functions

-export([
    ingest_logs_as_ndjson/5
]).

-define(chunk_size, 100).

%% API

-spec ingest_logs_as_ndjson(Req, App, Component, Branch, Version) -> Result when
    Req :: cowboy_req:req(),
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Result :: {data_protocol:generic_response(), cowboy_req:req()}.

ingest_logs_as_ndjson(Req, App, Component, Branch, Version) ->
    %% Сheck app, component, branch and version
    ?doif(util_binary:trim(App) =:= <<>>, web_rest_ingest_ndjson:ingest_logs_as_ndjson_400(#bad_request_error{reason = invalid_app})),
    ?doif(util_binary:trim(Component) =:= <<>>, web_rest_ingest_ndjson:ingest_logs_as_ndjson_400(#bad_request_error{reason = invalid_component})),
    ?doif(util_binary:trim(Branch) =:= <<>>, web_rest_ingest_ndjson:ingest_logs_as_ndjson_400(#bad_request_error{reason = invalid_branch})),
    ?doif(util_binary:trim(Version) =:= <<>>, web_rest_ingest_ndjson:ingest_logs_as_ndjson_400(#bad_request_error{reason = invalid_version})),

    % Read body
    {ok, Body, Req1} = web_util:collect_body(Req),

    % Parse NDJSON and convert to logs
    JsonArray = parse_ndjson(Body),
    Logs = [ingestion_protocol:ingested_log_entry_from_json(Elem) || Elem <- JsonArray],

    %% Check log entries
    ?doif(Logs =:= [], web_rest_ingest_ndjson:ingest_logs_as_ndjson_400(#bad_request_error{reason = no_logs_to_ingest})),

    % Group log entries into chunks
    Chunks = util_lists:to_chunks([
        % NB: allow only maps as data
        {TimeStamp, Level, ?yesno(is_map(Data), Data, #{}), Message, MsgCount, SeqId}
        ||
        #ingested_log_entry{
            data = Data,
            level = Level,
            message = Message,
            msg_count = MsgCount,
            seq_id = SeqId,
            timestamp = TimeStamp
        } <- Logs
    ], ?chunk_size),

    %% Save groups of log entries to database
    [db_if_logs:create_many(App, Component, Branch, Version, Chunk) || Chunk <- Chunks],

    {#generic_response{result = true}, Req1}.

%% Local functions

parse_ndjson(Data) ->
    Matches = binary:matches(Data, [<<"\r\n">>, <<"\n">>], []),
    parse_ndjson(Data, 0, Matches, []).

parse_ndjson(Data, Index, [], Acc) when Index >= byte_size(Data) ->
    lists:reverse([web_util:decode_json(Elem) || Elem <- Acc]);

parse_ndjson(Data, Index, [] = Matches, Acc) ->
    Elem = binary:part(Data, Index, byte_size(Data) - Index),
    parse_ndjson(Data, Index + byte_size(Elem), Matches, [Elem | Acc]);

parse_ndjson(Data, Index, [{Index, Length} | RestMatches], Acc) ->
    parse_ndjson(Data, Index + Length, RestMatches, Acc);

parse_ndjson(Data, Index, [{Pos, Length} | RestMatches], Acc) ->
    Elem = binary:part(Data, Index, Pos - Index),
    parse_ndjson(Data, Pos + Length, RestMatches, [Elem | Acc]).


%% Local tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_ndjson_single_line_test() ->
    ?assertEqual(
        parse_ndjson(<<"{\"a\": 1}">>),
        [#{<<"a">> => 1}]
    ),
    ok.

parse_ndjson_multi_lines_test() ->
    ?assertEqual(
        parse_ndjson(<<"{\"a\": 1}\r\n{\"a\": 2}\n{\"a\": 3}">>),
        [#{<<"a">> => 1},#{<<"a">> => 2},#{<<"a">> => 3}]
    ),
    ok.

parse_ndjson_multi_lines_with_breaks_test() ->
    ?assertEqual(
        parse_ndjson(<<"{\"a\": 1}\r\n{\"a\": 2}\n\n\r\n\n\n\n\n{\"a\": 3}">>),
        [#{<<"a">> => 1},#{<<"a">> => 2},#{<<"a">> => 3}]
    ),
    ok.

-endif.
