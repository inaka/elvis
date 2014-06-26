-module(elvis_utils).

-export([
         src/2,
         find_file/2,
         rules/1,
         source_dirs/1,
         validate_config/1,
         check_lines/3
        ]).

-type rule() :: mfa() | {Function :: atom(), Args :: list()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the source for the file. If the file
%% is not found tries to get the source from the compile
%% bytecode.
-spec src(elvis:config(), file:filename()) ->
    {ok, binary()} | not_found.
src(_Config, FilePath) ->
    file:read_file(FilePath).

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_file(string(), string()) -> [string()].
find_file(Path, Name) ->
    filelib:wildcard(Path ++ "**/" ++ Name).

%% @doc Reads the rules to apply specified in the
%%  configuration and returns them.
-spec rules(elvis:config()) -> [rule()].
rules(Config) ->
    proplists:get_value(rules, Config).

%% @doc Reads the source code directories specified in
%% the configuration and returns them.
-spec source_dirs(elvis:config()) -> [string()].
source_dirs(Config) ->
    proplists:get_value(src_dirs, Config).

%% @doc Checks that the configuration has all the required keys.
-spec validate_config(elvis:config()) -> valid | invalid.
validate_config(Config) ->
    RequiredKeys = [src_dirs, rules],
    validate_config(RequiredKeys, Config).

%% @private.
-spec validate_config([atom()], elvis:config()) -> valid | invalid.
validate_config([], _Config) ->
    valid;
validate_config([Key | Keys], Config) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            invalid;
        _ ->
            validate_config(Keys, Config)
    end.

%% @doc Takes a binary that holds source code and applies
%% Fun to each line. Fun takes 3 arguments (the line
%% as a binary, the line number and the supplied Args) and
%% returns 'no_result' or {'ok', Result}.
-spec check_lines(binary(), fun(), [term()]) ->
    [elvis_result:item_result()].
check_lines(Src, Fun, Args) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    check_lines(Lines, Fun, Args, [], 1).

%% @private
check_lines([], _Fun, _Args, Results, _Num) ->
    lists:reverse(Results);
check_lines([Line | Lines], Fun, Args, Results, Num) ->
    case Fun(Line, Num, Args) of
        {ok, Result} ->
            check_lines(Lines, Fun, Args, [Result | Results], Num + 1);
        no_result ->
            check_lines(Lines, Fun, Args, Results, Num + 1)
    end.
