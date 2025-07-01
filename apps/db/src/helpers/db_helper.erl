-module(db_helper).

%% Exported functions

-export([
    check_migration/0,
    setup_db/0,
    patch_db/0,
    actualize_db/0
]).

%% API

-spec check_migration() ->
    {'ok', CurrentVersion :: non_neg_integer()}
    | 'uninitialized'
    | {'outdated', CurrentVersion :: non_neg_integer(), LastVersion :: non_neg_integer()}
    | {'overdated', CurrentVersion :: non_neg_integer(), LastVersion :: non_neg_integer()}.

check_migration() ->
    {ok, CurrentVersion} = db_setup:db_version(),
    {ok, LastVersion} = db_config:last_version(),
    if
        CurrentVersion =< 0 -> uninitialized;
        CurrentVersion < LastVersion -> {outdated, CurrentVersion, LastVersion};
        CurrentVersion =:= LastVersion -> {ok, CurrentVersion};
        CurrentVersion > LastVersion -> {overdated, CurrentVersion, LastVersion}
    end.

-spec setup_db() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: term()}.

setup_db() ->
    case db_setup:setup() of
        {ok, Version} ->
            logger:info("=== SUCCESS! Database version ~p initialized successfully", [Version], #{caption => ?MODULE}),
            {ok, Version};
        {error, Reason} ->
            logger:error("=== ERROR! Can't initialize DB: ~p", [Reason], #{caption => ?MODULE}),
            {error, Reason}
    end.

-spec patch_db() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: term()}.

patch_db() ->
    logger:info("=== Updating DB schema...", #{caption => ?MODULE}),
    case db_setup:migrate() of
        {updated, OldVersion, NewVersion} ->
            logger:info("=== UPDATED! Updated from version ~p to ~p", [OldVersion, NewVersion], #{caption => ?MODULE}),
            {ok, NewVersion};
        {latest, Version} ->
            logger:info("=== LATEST! DB is at latest version: ~p", [Version], #{caption => ?MODULE}),
            {ok, Version};
        {error, Reason} ->
            logger:error("=== ERROR! Can't update DB: ~p", [Reason], #{caption => ?MODULE}),
            {error, Reason}
    end.

-spec actualize_db() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: term()}.

actualize_db() ->
    case check_migration() of
        uninitialized ->
            setup_db();
        {outdated, _CurrentVersion, _LastVersion} ->
            patch_db();
        {overdated, CurrentVersion, LastVersion} ->
            logger:error("=== ERROR! Can't update DB: ~p -> ~p (current version is higher)", [CurrentVersion, LastVersion], #{caption => ?MODULE}),
            {error, overdated};
        {ok, CurrentVersion} ->
            logger:info("=== LATEST! DB is at latest version: ~p", [CurrentVersion], #{caption => ?MODULE}),
            {ok, CurrentVersion}
    end.

%% Local functions
