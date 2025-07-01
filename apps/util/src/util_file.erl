-module(util_file).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    safe_delete/1,          % Try to delete regular file in a safe fashion
    rm_rf/1                 % Delete directory recursively
]).

%% API

-spec safe_delete(FilePath :: binary()) -> boolean().

safe_delete(FilePath) ->
    ?yesno(filelib:is_regular(FilePath), file:delete(FilePath) =:= ok, false).

-spec rm_rf(Dir :: file:filename_all()) ->
    'ok' | {'error', Reason :: atom()}.

rm_rf(Dir) ->
    Dir1 = ?yesno(is_binary(Dir), binary_to_list(Dir), Dir),
    Paths = filelib:wildcard(Dir1 ++ "/**"),
    {Dirs, Files} = lists:partition(fun filelib:is_dir/1, Paths),
    ok = lists:foreach(fun file:delete/1, Files),
    Sorted = lists:reverse(lists:sort(Dirs)),
    ok = lists:foreach(fun file:del_dir/1, Sorted),
    file:del_dir(Dir).

%% Local functions
