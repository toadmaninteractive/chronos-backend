-module(web_rest_callback_ingest).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("data_protocol.hrl").
-include("ingestion_protocol.hrl").

%% Exported functions

-export([
    ingest_logs/5
]).

-define(chunk_size, 100).

%% API

-spec ingest_logs(Logs, App, Component, Branch, Version) -> Result when
    Logs :: [ingestion_protocol:ingested_log_entry()],
    App :: binary(),
    Component :: binary(),
    Branch :: binary(),
    Version :: binary(),
    Result :: data_protocol:generic_response().

ingest_logs(Logs, App, Component, Branch, Version) ->

    %% check app, component, branch and version
    ?doif(util_binary:trim(App) =:= <<>>, web_rest_ingest:ingest_logs_400(#bad_request_error{reason = invalid_app})),
    ?doif(util_binary:trim(Component) =:= <<>>, web_rest_ingest:ingest_logs_400(#bad_request_error{reason = invalid_component})),
    ?doif(util_binary:trim(Branch) =:= <<>>, web_rest_ingest:ingest_logs_400(#bad_request_error{reason = invalid_branch})),
    ?doif(util_binary:trim(Version) =:= <<>>, web_rest_ingest:ingest_logs_400(#bad_request_error{reason = invalid_version})),

    %% check log entries
    ?doif(length(Logs) =:= 0, web_rest_ingest:ingest_logs_400(#bad_request_error{reason = no_logs_to_ingest})),

    %% group log entries to chunks
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

    %% save groups of log entries to database
    [db_if_logs:create_many(App, Component, Branch, Version, Chunk) || Chunk <- Chunks],

    #generic_response{result = true}.

%% Local functions
