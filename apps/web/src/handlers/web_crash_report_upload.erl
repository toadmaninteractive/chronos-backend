-module(web_crash_report_upload).
-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("http.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    init/2
]).

-define(chunk_size, 16 * 1024 * 1024).
-define(execOrThrow(Statement, ThrowArg), try (Statement) catch _:_:_ -> erlang:throw(ThrowArg) end).

-record(upload_file, {
    filename :: filelib:filename_all(),
    content_type :: binary(),
    tmp_path :: binary(),
    chunk_size = ?chunk_size :: non_neg_integer(),
    recv_size = 0 :: non_neg_integer(),
    max_size = ?chunk_size :: non_neg_integer(),
    md5_state :: crypto:hash_state(),
    md5 :: binary()
}).

-record(upload_state, {
    params = #{} :: maps:map(Param :: binary(), Value :: binary()),
    files = #{} :: maps:map(Param :: binary(), #upload_file{}),
    uploads_dir :: filelib:filename_all(),
    chunk_size = ?chunk_size :: non_neg_integer(),
    max_upload_size = ?chunk_size :: non_neg_integer(),
    recv_size = 0 :: non_neg_integer()
}).

%% API

init(Req, Opts) ->
    % Get configuration data for uploads
    {ok, MaxUploadSize} = web_config:max_upload_size(),
    {ok, TmpDir} = web_config:tmp_dir(),
    {ok, UploadsDir} = web_config:uploads_dir(),
    {ok, ApiKey} = db_if_settings:api_key(),

    % Read query parameters
    App = cowboy_req:binding(app, Req),
    Component = cowboy_req:binding(component, Req),
    Branch = cowboy_req:binding(branch, Req),
    Qs = maps:from_list(cowboy_req:parse_qs(Req)),
    Version = maps:get(<<"version">>, Qs, undefined),
    SuppliedApiKey = cowboy_req:header(<<"x-api-key">>, Req),

    Req1 = try
        % Check API key
        ?assert(is_binary(SuppliedApiKey) andalso SuppliedApiKey =:= ApiKey, invalid_api_key),

        % Check app, version and branch
        ?assert(is_binary(App) andalso App =/= <<>>, invalid_app),
        ?assert(is_binary(Version) andalso Version =/= <<>>, invalid_version),
        ?assert(is_binary(Branch) andalso Branch =/= <<>>, invalid_branch),

        % Read multipart request
        InitialUploadState = #upload_state{chunk_size = ?chunk_size, max_upload_size = MaxUploadSize, uploads_dir = TmpDir},
        {R1, #upload_state{files = Files, params = Params}} = read_multipart(Req, InitialUploadState),

        % Extract creash dump information
        GameEngine = maps:get(<<"game_engine">>, Params, undefined),
        Username = maps:get(<<"username">>, Params, undefined),
        Reason = maps:get(<<"reason">>, Params, undefined),
        UserComment = maps:get(<<"user_comment">>, Params, undefined),
        Data = jsx:decode(maps:get(<<"data">>, Params, <<"{}">>), [return_maps]),

        % Check game engine, username and reason
        ?assert(is_binary(GameEngine) andalso GameEngine =/= <<>>, invalid_game_engine),
        ?assert(is_binary(Username) andalso Username =/= <<>>, invalid_username),
        ?assert(is_binary(Reason) andalso Reason =/= <<>>, invalid_reason),

        % Get crash dump archive
        #upload_file{
            filename = Filename,
            tmp_path = TmpPath,
            md5 = Md5
        } = ?execOrThrow(#upload_file{} = maps:get(<<"dump_archive">>, Files), invalid_filename),

        % Move crash report archive
        TargetFilename = <<Md5/binary, "_", Filename/binary>>,
        TargetPath = filename:join([UploadsDir, App, TargetFilename]),
        filelib:ensure_dir(TargetPath),
        file:rename(TmpPath, TargetPath),

        % Delete all temporary files
        maps:fold(fun(_, #upload_file{tmp_path = TP}, _) -> file:delete(TP) end, [], Files),

        % Add crash dump information to database
        case db_if_crash_reports:create(App, Component, Branch, Version, GameEngine, Username, TargetFilename, Reason, UserComment, Data) of
            {ok, ReportId} -> success(R1, #{result => ok, report_id => ReportId});
            {error, Error} -> internal_server_error(Req, Error)
        end
    catch
        % 400
        throw:invalid_app:_ -> bad_request(Req, invalid_app);
        throw:invalid_version:_ -> bad_request(Req, invalid_version);
        throw:invalid_branch:_ -> bad_request(Req, invalid_branch);
        throw:invalid_game_engine:_ -> bad_request(Req, invalid_game_engine);
        throw:invalid_username:_ -> bad_request(Req, invalid_username);
        throw:invalid_reason:_ -> bad_request(Req, invalid_reason);
        throw:invalid_filename:_ -> bad_request(Req, invalid_filename);

        % 403
        throw:invalid_api_key:_ -> forbidden(Req, invalid_api_key);

        % 500
        Type:What:StackTrace ->
            logger:error("Failed to upload crash report (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
            internal_server_error(Req, What)
    end,
    {ok, Req1, Opts}.

%% Local functions

tmp_filepath(PathPrefix) ->
    Uuid = util_hex:from_binary(uuid:get_v4(strong)),
    filename:join([PathPrefix, Uuid]).

read_multipart(Req, #upload_state{uploads_dir = UploadsDir, chunk_size = ChunkSize, max_upload_size = MaxSize, recv_size = RecvSize, files = Files, params = Params} = State) ->
    case cowboy_req:read_part(Req, #{length => MaxSize}) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, Param} ->
                    {ok, Value, R1} = cowboy_req:read_part_body(Req1),
                    read_multipart(R1, State#upload_state{params = Params#{Param => Value}});
                {file, Param, FileName, ContentType} ->
                    TmpFilePath = tmp_filepath(UploadsDir),
                    file:delete(TmpFilePath),
                    InitialState = #upload_file{
                        filename = FileName,
                        content_type = ContentType,
                        tmp_path = TmpFilePath,
                        chunk_size = ChunkSize,
                        max_size = MaxSize,
                        md5_state = crypto:hash_init(md5),
                        md5 = <<>>
                    },
                    {R1, #upload_file{recv_size = RecvFileSize} = F} = recv_file(Req1, InitialState),
                    read_multipart(R1, State#upload_state{files = Files#{Param => F}, recv_size = RecvSize + RecvFileSize})
            end;
        {done, Req1} ->
            {Req1, State}
    end.

recv_file(Req, #upload_file{recv_size = RecvSize, max_size = _MaxSize, tmp_path = TmpPath, md5_state = Md5State} = State) ->
    case cowboy_req:read_part_body(Req, #{length => ?chunk_size, period => 7200}) of
        {ok, Data, Req1} ->
            write_file_chunk(TmpPath, Data),
            Md5State1 = crypto:hash_update(Md5State, Data),
            Md5 = util_hex:from_binary(crypto:hash_final(Md5State1)),
            {Req1, State#upload_file{recv_size = RecvSize + byte_size(Data), md5 = Md5, md5_state = undefined}};
        {more, Data, Req1} ->
            write_file_chunk(TmpPath, Data),
            Md5State1 = crypto:hash_update(Md5State, Data),
            recv_file(Req1, State#upload_file{recv_size = RecvSize + byte_size(Data), md5_state = Md5State1})
    end.

write_file_chunk(FilePath, Data) ->
    {ok, IoDevice} = file:open(FilePath, [append, raw, binary]),
    ok = file:write(IoDevice, Data),
    ok = file:close(IoDevice).

reply_json_explicit(Status, Json, Req) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    Body = jsx:encode(Json),
    cowboy_req:reply(Status, Headers, Body, Req).

success(Req, Json) ->
    reply_json_explicit(?http_ok, Json, Req).

bad_request(Req, Error) ->
    reply_json_explicit(?http_bad_request, #{error => Error}, Req).

forbidden(Req, Error) ->
    reply_json_explicit(?http_forbidden, #{error => Error}, Req).

not_found(Req, Error) ->
    reply_json_explicit(?http_not_found, #{error => Error}, Req).

internal_server_error(Req, Error) ->
    reply_json_explicit(?http_internal_server_error, #{error => Error}, Req).
