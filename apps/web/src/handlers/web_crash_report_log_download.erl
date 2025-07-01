-module(web_crash_report_log_download).
-behaviour(cowboy_handler).

%% Include files

-include_lib("stdlib/include/zip.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("http.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    % Get queried file
    {ok, UploadsDir} = web_config:uploads_dir(),
    App = cowboy_req:binding(app, Req),
    FileName = cowboy_req:binding(filename, Req),
    FilePath = binary_to_list(filename:join([UploadsDir, App, FileName])),
    ReplyHeaders = #{<<"Content-Type">> => <<"text/plain">>},

    % Try open ZIP archive
    Req1 = case zip:zip_open(FilePath, [memory]) of
        {ok, ZipHandle} ->
            ReqFinal = try
                {ok, ZipContents} = zip:zip_list_dir(ZipHandle),
                ArchivedFiles = [File || #zip_file{name = File} <- ZipContents],
                LogFiles = [File || File <- ArchivedFiles, is_log_file(util_binary:to_lower(iolist_to_binary(File)))],
                BestMatch = case LogFiles of [File | _] -> File; _ -> undefined end,
                case BestMatch of
                    undefined ->
                        logger:error("failed to locate log file. Application: <~s>, creash report: <~s>", [App, FileName], #{caption => ?MODULE});
                    _ ->
                        {ok, {_, LogFileContents}} = zip:zip_get(BestMatch, ZipHandle),
                        cowboy_req:reply(200, ReplyHeaders, LogFileContents, Req)
                end
            catch _:_:_ ->
                cowboy_req:reply(500, ReplyHeaders, <<"Failed to unpack archive">>, Req)
            end,
            zip:zip_close(ZipHandle),
            ReqFinal;
        {error, _Reason} ->
            cowboy_req:reply(404, ReplyHeaders, <<"Crash report archive not found">>, Req)
    end,
    {ok, Req1, Opts}.

%% Local functions

is_log_file(Filename) when is_binary(Filename), binary_part(Filename, byte_size(Filename), -4) =:= <<".log">> -> true;
is_log_file(_) -> false.
