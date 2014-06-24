-module(elvis_utils).

-export([
         src/2,
         find_file/2,
         rules/1,
         source_dirs/1
        ]).

-define(DEFAULT_SOURCE_DIRS, ["src"]).

-type rule() :: mfa() | {Function :: atom(), Args :: list()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec src(elvis:config(), file:filename()) ->
    {ok, binary()} | not_found.
src(_Config, FilePath) ->
    file:read_file(FilePath).

-spec find_file(string(), string()) -> [string()].
find_file(Path, Name) ->
    filelib:wildcard(Path ++ "**/" ++ Name).

-spec rules(elvis:config()) -> [rule()].
rules(Config) ->
    proplists:get_value(rules, Config).

-spec source_dirs(elvis:config()) -> [string()].
source_dirs(Config) ->
    case proplists:get_value(src_dirs, Config) of
        undefined ->
            ?DEFAULT_SOURCE_DIRS;
        Dirs ->
            Dirs
    end.
