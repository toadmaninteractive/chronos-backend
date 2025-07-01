-module(web_rest_callback_apps).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_applications/0
]).

%% API

-spec get_applications() ->
    [web_protocol:application()].

get_applications() ->
    {ok, Items} = db_if_app_cache:structure(),
    Apps = ordsets:from_list([App || #{<<"app">> := App} <- Items]),
    [#application{
        name = App,
        components = [#component{name = Component, branches = Bs, versions = Vs} || {Component, Bs, Vs} <- components(App, Items, true)]
    } || App <- Apps].

%% Local functions

components(App, Items, WithVersions) ->
    Components = ordsets:from_list([C || #{<<"app">> := A, <<"component">> := C} <- Items, A =:= App]),
    [begin
        RequiredPart = [Component, branches(App, Component, Items)],
        list_to_tuple(RequiredPart ++ ?yesno(WithVersions, [versions(App, Component, Items)], []))
    end || Component <- Components].

branches(App, Component, Items) ->
    ordsets:from_list([B || #{<<"app">> := A, <<"component">> := C, <<"branch">> := B} <- Items, A =:= App, C =:= Component]).

versions(App, Component, Items) ->
    ordsets:from_list([V || #{<<"app">> := A, <<"component">> := C, <<"version">> := V} <- Items, A =:= App, C =:= Component]).
