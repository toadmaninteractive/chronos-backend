%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(protocol_chronos).

-include_lib("stdlib/include/assert.hrl").
-include("protocol_chronos.hrl").

-export([
    log_entry_to_json/1,
    log_entry_from_json/1,
    invalid_data_to_json/1,
    invalid_data_from_json/1
]).

-export_type([
    log_entry/0,
    invalid_data/0
]).

-type log_entry() :: #log_entry{}.
-type invalid_data() :: #invalid_data{}.

-spec log_entry_to_json(log_entry()) -> igor_json:json_object().

log_entry_to_json(Record) ->
    #log_entry{
        level = Level,
        timestamp = Timestamp,
        data = Data,
        message = VarMessage,
        msg_count = MsgCount,
        seq_id = SeqId
    } = Record,
    ?assert(Level =/= undefined),
    ?assert(Timestamp =/= undefined),
    ?assert(Data =/= undefined),
    ?assert(VarMessage =/= undefined),
    ?assert(MsgCount =/= undefined),
    ?assert(SeqId =/= undefined),
    #{
        <<"level">> => igor_json:pack(Level, {custom, fun data_protocol:log_level_to_json/1}),
        <<"timestamp">> => igor_json:pack(Timestamp, {custom, fun web_types:datetime_to_text/1}),
        <<"data">> => igor_json:pack(Data, json),
        <<"message">> => igor_json:pack(VarMessage, string),
        <<"msg_count">> => igor_json:pack(MsgCount, int),
        <<"seq_id">> => igor_json:pack(SeqId, int)
    }.

-spec log_entry_from_json(igor_json:json_object()) -> log_entry().

log_entry_from_json(Json) ->
    Level = igor_json:parse(Json, <<"level">>, {custom, fun data_protocol:log_level_from_json/1}),
    Timestamp = igor_json:parse(Json, <<"timestamp">>, {custom, fun web_types:datetime_from_text/1}),
    Data = igor_json:parse(Json, <<"data">>, json),
    VarMessage = igor_json:parse(Json, <<"message">>, string),
    MsgCount = igor_json:parse(Json, <<"msg_count">>, int),
    SeqId = igor_json:parse(Json, <<"seq_id">>, int),
    #log_entry{
        level = Level,
        timestamp = Timestamp,
        data = Data,
        message = VarMessage,
        msg_count = MsgCount,
        seq_id = SeqId
    }.

-spec invalid_data_to_json(invalid_data()) -> igor_json:json_object().

invalid_data_to_json(Record) ->
    #invalid_data{reason = VarReason} = Record,
    ?assert(VarReason =/= undefined),
    #{
        <<"reason">> => igor_json:pack(VarReason, string)
    }.

-spec invalid_data_from_json(igor_json:json_object()) -> invalid_data().

invalid_data_from_json(Json) ->
    VarReason = igor_json:parse(Json, <<"reason">>, string),
    #invalid_data{reason = VarReason}.

