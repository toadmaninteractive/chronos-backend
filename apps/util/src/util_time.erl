-module(util_time).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    utc_nanoseconds/0,      % UTC UNIX timestamp (POSIX nanoseconds)
    utc_microseconds/0,     % UTC UNIX timestamp (POSIX microseconds)
    utc_milliseconds/0,     % UTC UNIX timestamp (POSIX milliseconds)
    utc_seconds/0,          % UTC UNIX timestamp (POSIX seconds)
    utc_datetime/0,         % UTC date and time
    local_datetime/0,       % Local date and time
    datetime_to_seconds/1,  % Converts Erlang date_time() term to Unix timestamp (number seconds passed from 00:00:00 01.01.1970)
    seconds_to_datetime/1,  % Converts Unix timestamp (number seconds passed from 00:00:00 01.01.1970) to Erlang date_time() term
    time_to_day_end/0,      % Seconds before this day end
    time_to_week_end/0,     % Seconds before this week end
    time_to_month_end/0,    % Seconds before this month end
    iso_8601_fmt/1,         % Format date and time according to ISO-8601 standard
    iso_8601_fmt_tz/2,      % Format date and time according to ISO-8601 standard
    parse_8601_datetime/1   % Parse ISO-8601 datetime with milliseconds
]).

-type nanoseconds() :: integer().
-type microseconds() :: integer().
-type milliseconds() :: integer().
-type seconds() :: 0..59.
-type minutes() :: 0..59.
-type hours() :: 0..23.
-type days() :: integer().
-type time() :: number().
-type span() :: number().
-type zone() :: {integer(), minutes()}.
-type timems() :: {hours(), minutes(), float()}.
-type datetimems() :: {calendar:date(), timems()}.

-export_type([
    nanoseconds/0,
    microseconds/0,
    milliseconds/0,
    seconds/0,
    minutes/0,
    hours/0,
    days/0,
    time/0,
    span/0,
    zone/0,
    timems/0,
    datetimems/0
]).

-record(iso_8601_parse_state, {
    year = 0    :: non_neg_integer(),
    month = 1   :: 1..12,
    day = 1     :: 1..31,
    hour = 0    :: 0..23,
    minute = 0  :: 0..59,
    second = 0  :: 0..59,
    milisecond = 0 :: non_neg_integer(),
    zone = {0, 0} :: zone(),
    separator = undefined :: boolean() | 'undefined'
}).

-define(date_fmt, "~4.10.0B-~2.10.0B-~2.10.0B").
-define(time_fmt, "~2.10.0B:~2.10.0B:~2.10.0B").

%% API

-spec utc_datetime() -> calendar:datetime().

utc_datetime() ->
    erlang:universaltime().

-spec local_datetime() -> calendar:datetime().

local_datetime() ->
    erlang:localtime().

-spec utc_nanoseconds() -> nanoseconds().

utc_nanoseconds() ->
    erlang:system_time(nano_seconds).

-spec utc_microseconds() -> microseconds().

utc_microseconds() ->
    erlang:system_time(micro_seconds).

-spec utc_milliseconds() -> milliseconds().

utc_milliseconds() ->
    erlang:system_time(milli_seconds).

-spec utc_seconds() -> seconds().

utc_seconds() ->
    erlang:system_time(seconds).

-spec datetime_to_seconds(calendar:datetime()) -> seconds().

datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

-spec seconds_to_datetime(seconds()) -> calendar:datetime().

seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

-spec time_to_day_end() -> seconds().

time_to_day_end() ->
    {_Date, Time} = calendar:universal_time(),
    TimeSeconds = calendar:time_to_seconds(Time),
    3600 * 24 - TimeSeconds.

-spec time_to_week_end() -> seconds().

time_to_week_end() ->
    {Date, Time} = calendar:universal_time(),
    Day = calendar:day_of_the_week(Date),
    TimeSeconds = calendar:time_to_seconds(Time),
    (8 - Day) * 3600 * 24 - TimeSeconds.

-spec time_to_month_end() -> seconds().

time_to_month_end() ->
    {Date, Time} = calendar:universal_time(),
    {Year, Month, Day} = Date,
    LastDay = calendar:last_day_of_the_month(Year, Month),
    TimeSeconds = calendar:time_to_seconds(Time),
    (LastDay - Day + 1) * 3600 * 24 - TimeSeconds.

-spec iso_8601_fmt(calendar:datetime() | {'datetime', calendar:datetime()}) -> binary().

iso_8601_fmt({datetime, DateTime}) ->
    iso_8601_fmt(DateTime);
iso_8601_fmt({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    IoList = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]),
    iolist_to_binary(IoList).

-spec iso_8601_fmt_tz(calendar:datetime() | {'datetime', calendar:datetime()}, TimeZone :: 'utc' | {HH :: 0..23, MM :: 0..59}) -> binary().

iso_8601_fmt_tz({datetime, DateTime}, TimeZone) ->
    iso_8601_fmt_tz(DateTime, TimeZone);
iso_8601_fmt_tz({{Year, Month, Day}, {Hour, Min, Sec}}, TimeZone) ->
    DateBlock = iolist_to_binary(io_lib:format(?date_fmt, [Year, Month, Day])),
    TimeBlock = case Sec of
        Integer when is_integer(Integer) -> iolist_to_binary(io_lib:format(?time_fmt, [Hour, Min, Sec]));
        Float when is_float(Float) -> iolist_to_binary(io_lib:format(?time_fmt ++ ".~3.10.0B", [Hour, Min, trunc(Sec), trunc(Sec * 1000) rem 1000]))
    end,
    TzBlock = tz_to_binary(TimeZone),
    <<DateBlock/binary, "T", TimeBlock/binary, TzBlock/binary>>.

-spec parse_8601_datetime(DateTime :: binary()) -> datetimems().

parse_8601_datetime(DateTime) ->
    State = year(DateTime, #iso_8601_parse_state{}),
    to_datetimems(State).

%% Local functions

tz_to_binary(utc) -> <<"Z">>;
tz_to_binary({0, 0}) -> <<"Z">>;
tz_to_binary({HH, 0}) -> list_to_binary(io_lib:format("~c~2.10.0B", [?yesno(HH < 0, $-, $+), abs(HH)]));
tz_to_binary({HH, MM}) -> list_to_binary(io_lib:format("~c~2.10.0B:~2.10.0B", [?yesno(HH < 0, $-, $+), abs(HH), MM])).

to_datetimems(State) -> {to_date(State), to_timems(State)}.
to_date(#iso_8601_parse_state{year = Y, month = M, day = D}) -> {Y, M, D}.
to_timems(#iso_8601_parse_state{hour = H, minute = M, second = S, milisecond = MS}) -> {H, M, S + MS * 0.001}.

make_int([], _Power, Acc) ->
    Acc;
make_int([A|Tail], Power, Acc) when $0 =< A, A =< $9 ->
    make_int(Tail, Power * 10, Acc + Power * (A - $0));
make_int(_A, _, _) ->
    error(badarg).

year(<<Y1, Y2, Y3, Y4, Rest/binary>>, Acc) ->
    Year = make_int([Y4, Y3, Y2, Y1], 1, 0),
    month(Rest, Acc#iso_8601_parse_state{year = Year});
year(_, _) ->
    error(badarg).

month(<<$-, M1, M2, Rest/binary>>, Acc) ->
    Month = make_int([M2, M1], 1, 0),
    day(Rest, Acc#iso_8601_parse_state{month = Month, separator = true});
month(<<M1, M2, Rest/binary>>, Acc) ->
    Month = make_int([M2, M1], 1, 0),
    day(Rest, Acc#iso_8601_parse_state{month = Month, separator = false});
month(_, _) ->
    error(badarg).

day(<<$-, D1, D2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =:= true ->
    Day = make_int([D2, D1], 1, 0),
    hour_join(Rest, Acc#iso_8601_parse_state{day = Day});
day(<<D1, D2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =:= false ->
    Day = make_int([D2, D1], 1, 0),
    hour_join(Rest, Acc#iso_8601_parse_state{day = Day});
day(<<>>, Acc) ->
    Acc;
day(_, _) ->
    error(badarg).

hour_join(<<$T, Rest/binary>>, Acc) ->
    hour(Rest, Acc);
hour_join(Rest, Acc) ->
    rest(Rest, Acc).

hour(<<H1, H2, Rest/binary>>, Acc) ->
    Hour = make_int([H2, H1], 1, 0),
    minute(Rest, Acc#iso_8601_parse_state{hour = Hour});
hour(_, _) ->
    error(badarg).

minute(<<$:, M1, M2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =/= false ->
    Minute = make_int([M2, M1], 1, 0),
    second(Rest, Acc#iso_8601_parse_state{minute = Minute, separator = true});
minute(<<M1, M2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =/= true ->
    Minute = make_int([M2, M1], 1, 0),
    second(Rest, Acc#iso_8601_parse_state{minute = Minute, separator = false});
minute(_, _) ->
    error(badarg).

second(<<$:, S1, S2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =:= true ->
    Second = make_int([S2, S1], 1, 0),
    milisecond(Rest, Acc#iso_8601_parse_state{second = Second});
second(<<S1, S2, Rest/binary>>, Acc) when Acc#iso_8601_parse_state.separator =:= false ->
    Second = make_int([S2, S1], 1, 0),
    rest(Rest, Acc#iso_8601_parse_state{second = Second});
second(<<>>, Acc) ->
    Acc;
second(_, _) ->
    error(badarg).

milisecond(<<$., S1, S2, S3, Rest/binary>>, Acc) ->
    Milisecond = make_int([S3, S2, S1], 1, 0),
    rest(Rest, Acc#iso_8601_parse_state{milisecond = Milisecond});
milisecond(Rest, Acc) ->
    rest(Rest, Acc).

rest(<<Sign, H1, H2, $:, M1, M2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#iso_8601_parse_state{zone = make_zone(Sign, H1, H2, M1, M2)};
rest(<<Sign, H1, H2, M1, M2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#iso_8601_parse_state{zone = make_zone(Sign, H1, H2, M1, M2)};
rest(<<Sign, H1, H2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#iso_8601_parse_state{zone = make_zone(Sign, H1, H2, $0, $0)};
rest(<<$Z>>, Acc) ->
    Acc#iso_8601_parse_state{zone = {0, 0}};
rest(<<>>, Acc) ->
    Acc;
rest(_Str, _Acc) ->
    error(badarg).

make_zone(Sign, H1, H2, M1, M2) ->
    HH = make_int([H2, H1], 1, 0),
    MM = make_int([M2, M1], 1, 0),
    {sign_to_int(Sign)*HH, MM}.

sign_to_int($+) -> 1;
sign_to_int($-) -> -1.
