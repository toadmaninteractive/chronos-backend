-module(db_query_builder).

%% Exported functions

-export([
    new/1,
    new/2,
    select/2,
    where/2,
    group_by/2,
    order_by/2,
    offset/2,
    limit/2,
    compile/1,
    where_opt/3,
    where_map/2,
    param/1
]).

-record(query, {
    table = undefined,
    select = [],
    where = [],
    group_by = undefined,
    order_by = undefined,
    offset = undefined,
    limit = undefined,
    params = []
}).

%% API

new(Table) ->
    #query{table = Table}.

new(Table, Select) ->
    #query{table = Table, select = Select}.

select(Query, Param) ->
    #query{select = Select} = Query,
    Query#query{select = Select ++ [Param]}.

param(Param) ->
    {Param}.

where_opt(Query, undefined, _Cond) ->
    Query;
where_opt(Query, _, Cond) ->
    where(Query, Cond).

where(Query, Cond0) ->
    Cond = lists:flatten(Cond0),
    #query{where = Where, params = Params} = Query,
    NewParams = lists:filtermap(fun
        ({Param}) -> {true, Param};
        ({Param, _Mods}) -> {true, Param};
        (_) -> false
    end, Cond),

    {FixCond, _} = lists:foldl(fun(Elem, {AccCond, NewIndex}) ->
        case Elem of
            {_} -> {AccCond ++ ["$" ++ integer_to_list(NewIndex)], NewIndex + 1};
            {_, Mods} when is_list(Mods) -> {AccCond ++ [apply_mods(Mods, "$" ++ integer_to_list(NewIndex))], NewIndex + 1};
            Elem -> {AccCond ++ [Elem], NewIndex}
        end
    end, {"", length(Params) + 1}, Cond),

    Query#query{where = Where ++ [FixCond], params = Params ++ NewParams}.

where_map(Query, Conds) ->
    maps:fold(fun
        (K, {V, Mods}, Q) -> where(Q, [K, " = ", {V, Mods}]);
        (K, V, Q) -> where(Q, [K, " = ", {V}])
    end, Query, Conds).

group_by(Query, GroupBy) ->
    Query#query{group_by = GroupBy}.

order_by(Query, OrderBy) ->
    Query#query{order_by = OrderBy}.

offset(Query, Offset) ->
    Query#query{offset = Offset}.

limit(Query, Limit) ->
    Query#query{limit = Limit}.

compile(Query) ->
    #query{table = Table, select = Select, where = Where, group_by = GroupBy, order_by = OrderBy, limit = Limit, offset = Offset, params = Params} = Query,
    {iolist_to_binary([
        [ "SELECT ", join(Select, ", "), " FROM ", Table ],
        if Where =:= [] ->
            [];
        true ->
            [ " WHERE ", join(Where, " AND ") ]
        end,
        if GroupBy =:= undefined ->
            [];
        true ->
            [ " GROUP BY ", GroupBy ]
        end,
        if OrderBy =:= undefined ->
            [];
        true ->
            [ " ORDER BY ", OrderBy ]
        end,
        if Limit =:= undefined ->
            [];
        true ->
            " LIMIT " ++ integer_to_list(Limit)
        end,
        if Offset =:= undefined ->
            [];
        true ->
            " OFFSET " ++ integer_to_list(Offset)
        end
    ]), Params}.

%% Local functions

join(List, Sep) ->
    [hd(List) | [[Sep, I] || I <- tl(List)]].

apply_mods([], Param) -> Param;
apply_mods([lower|T], Param) -> apply_mods(T, "LOWER(" ++ Param ++ ")");
apply_mods([upper|T], Param) -> apply_mods(T, "UPPER(" ++ Param ++ ")");
apply_mods([ltrim|T], Param) -> apply_mods(T, "LTRIM(" ++ Param ++ ")");
apply_mods([rtrim|T], Param) -> apply_mods(T, "RTRIM(" ++ Param ++ ")");
apply_mods([trim|T], Param) -> apply_mods(T, "TRIM(" ++ Param ++ ")");
apply_mods([_|T], Param) -> apply_mods(T, Param).
