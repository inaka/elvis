-module(elvis_test_utils).

-export([
         find_file/2
        ]).

-spec find_file([string()], string()) ->
    {ok, string()} | {error, enoent}.
find_file(Dirs, Pattern) ->
    case elvis_utils:find_files(Dirs, Pattern) of
        [] -> {error, enoent};
        [Path | _] -> {ok, Path}
    end.
