-module(slack_test).

%% Include files

-include("slack_protocol.hrl").

%% Exported functions

-export([
    d/0,
    i/0,
    w/0,
    e/0,
    test/3
]).

%% API

d() ->
    logger:debug("~s: test debug", [?MODULE]).

i() ->
    logger:info("~s: test info", [?MODULE]).

w() ->
    logger:warning("~s: test warning", [?MODULE]).

e() ->
    logger:error("~s: test error", [?MODULE]).

test(Kind, Num, IntervalSec) ->
    Fun = case Kind of
        d -> fun d/0;
        i -> fun i/0;
        w -> fun w/0;
        e -> fun e/0
    end,
    Period = round(IntervalSec * 1000 / Num),
    [begin Fun(), timer:sleep(Period) end || _ <- lists:seq(1, Num)].

%% Local functions
