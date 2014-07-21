-module(elvis_utils).

-export([
         src/2,

         %% Files
         find_files/1,
         find_files/2,
         is_erlang_file/1,
         filter_files/1,

         %% Rules
         check_lines/3,
         check_lines_with_context/4,
         indentation/3,
         check_nodes/3,

         %% General
         erlang_halt/1,
         to_str/1
        ]).

-export_type([file/0]).

-define(FILE_PATTERN, "*.erl").
-define(FILE_EXTENSIONS, [".erl"]).

-type file() :: #{path => string(), content => binary()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the source for the file.
-spec src(elvis_config:config(), file()) ->
    {ok, binary()} | {error, enoent}.
src(_Config, #{content := Content}) ->
    {ok, Content};
src(_Config, #{path := Path}) ->
    file:read_file(Path);
src(_Config, File) ->
    throw({invalid_file, File}).


%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [file()].
find_files(Dirs, Pattern) ->
    Fun = fun(Dir) ->
                filelib:wildcard(Dir ++ "/**/" ++ Pattern)
          end,
    [#{path => Path} || Path <- lists:flatmap(Fun, Dirs)].

-spec find_files([string()]) -> [file()].
find_files(Dirs) ->
    find_files(Dirs, ?FILE_PATTERN).

%% @doc Takes a binary that holds source code and applies
%% Fun to each line. Fun takes 3 arguments (the line
%% as a binary, the line number and the supplied Args) and
%% returns 'no_result' or {'ok', Result}.
-spec check_lines(binary(), fun(), [term()]) ->
    [elvis_result:item()].
check_lines(Src, Fun, Args) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    check_lines(Lines, Fun, Args, [], 1).

-type line_content() :: {integer(), integer()}.

-spec check_lines_with_context(binary(), fun(), [term()], line_content()) ->
    [elvis_result:item()].
check_lines_with_context(Src, Fun, Args, Ctx) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    LinesContext = context(Lines, Ctx),
    check_lines(LinesContext, Fun, Args, [], 1).

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

%% @private
context(List, CtxCount) ->
    context(List, [], CtxCount, []).

context([], _Past, _CtxCount, Results) ->
    lists:reverse(Results);
context([Current | Future], Past, CtxCount = {PrevCount, NextCount}, Results) ->
    Prev = lists:sublist(Past, PrevCount),
    Next = lists:sublist(Future, NextCount),
    Item = {Current, lists:reverse(Prev), Next},
    context(Future, [Current | Past], CtxCount, [Item | Results]).

%% @doc Takes a binary that holds source code and applies
%% Fun to each line. Fun takes 3 arguments (the line
%% as a binary, the line number and the supplied Args) and
%% returns 'no_result' or {'ok', Result}.
-spec check_nodes(elvis_code:tree_node(), fun(), [term()]) ->
    [elvis_result:item()].
check_nodes(RootNode, Fun, Args) ->
    ChildNodes = elvis_code:content(RootNode),
    check_nodes(ChildNodes, Fun, Args, []).

%% @private
check_nodes([], _Fun, _Args, Results) ->
    FlatResults = lists:flatten(Results),
    lists:reverse(FlatResults);
check_nodes([Node | Nodes], Fun, Args, Results) ->
    case Fun(Node, Args) of
        [] ->
            check_nodes(Nodes, Fun, Args, Results);
        Result ->
            check_nodes(Nodes, Fun, Args, [Result | Results])
    end.

%% @doc This is defined so tht it an be mocked for tests.
-spec erlang_halt(integer()) -> any().
erlang_halt(Code) ->
    halt(Code).

-spec to_str(binary() | list() | atom()) -> string().
to_str(Arg) when is_binary(Arg) ->
    binary_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.

-spec is_erlang_file(string()) -> true | false.
is_erlang_file(Path) ->
    Path1 = to_str(Path),
    lists:member(filename:extension(Path1), ?FILE_EXTENSIONS).

-spec filter_files([elvis_utils:file()]) -> [elvis_utils:file()].
filter_files(Files) ->
    [File || File = #{path := Path} <- Files,
             is_erlang_file(Path)].

%% @doc Takes a line, a character and a count, returning the indentation level
%%      invalid if the number of character is not a multiple of count.
-spec indentation(binary() | string(), char(), integer()) ->
    invalid | integer().
indentation(Line, Char, Count) ->
    LineStr = to_str(Line),
    Regex = "^" ++ [Char] ++ "*",
    {match, [{0, Len} | _]} = re:run(LineStr, Regex),
    case Len rem Count of
        0 -> Len div Count;
        _ -> invalid
    end.
