%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(common_protocol).

-include_lib("stdlib/include/assert.hrl").
-include("common_protocol.hrl").

-export([
    empty_to_json/1,
    empty_from_json/1
]).

-export_type([
    empty/0,
    id/0
]).

-type empty() :: #empty{}.
-type id() :: igor_types:uint().

-spec empty_to_json(empty()) -> igor_json:json_object().

empty_to_json(#empty{}) -> #{}.

-spec empty_from_json(igor_json:json_object()) -> empty().

empty_from_json(_Json) -> #empty{}.

