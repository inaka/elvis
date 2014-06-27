-module(elvis_utils).

-export([
         src/2,
         find_files/2,
         check_lines/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the source for the file.
-spec src(elvis_config:config(), file:filename()) ->
    {ok, binary()} | {error, enoent}.
src(_Config, FilePath) ->
    file:read_file(FilePath).

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [string()].
find_files(Dirs, Pattern) ->
    Fun = fun(Dir) ->
                filelib:wildcard(Dir ++ "**/" ++ Pattern)
          end,
    lists:flatmap(Fun, Dirs).

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
