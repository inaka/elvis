-module(elvis_utils).

-export([
         %% Rules
         check_lines/3,
         check_lines_with_context/4,
         indentation/3,
         check_nodes/3,

         %% General
         erlang_halt/1,
         to_str/1,

         %% Output
         info/1,
         info/2,
         notice/1,
         notice/2,
         error_prn/1,
         error_prn/2,
         parse_colors/1
        ]).

-export_type([file/0]).

-type file() :: #{path => string(), content => binary()}.
-type line_content() :: {integer(), integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Takes a binary that holds source code and applies
%%      Fun to each line. Fun takes 3 arguments (the line
%%      as a binary, the line number and the supplied Args) and
%%      returns 'no_result' or {'ok', Result}.
-spec check_lines(binary(), fun(), [term()]) ->
    [elvis_result:item()].
check_lines(Src, Fun, Args) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    check_lines(Lines, Fun, Args, [], 1).

%% @doc Checks each line calling fun and providing the previous and next
%%      lines based on the context tuple {Before, After}.
-spec check_lines_with_context(binary(), fun(), [term()], line_content()) ->
    [elvis_result:item()].
check_lines_with_context(Src, Fun, Args, Ctx) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    LinesContext = context(Lines, Ctx),
    check_lines(LinesContext, Fun, Args, [], 1).

%% @private
check_lines([], _Fun, _Args, Results, _Num) ->
    lists:flatten(lists:reverse(Results));
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
-spec check_nodes(ktn_code:tree_node(), fun(), [term()]) ->
    [elvis_result:item()].
check_nodes(RootNode, Fun, Args) ->
    ChildNodes = ktn_code:content(RootNode),
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

%% @doc This is defined so that it can be mocked for tests.
-spec erlang_halt(integer()) -> no_return().
erlang_halt(Code) ->
    halt(Code).

-spec to_str(binary() | list() | atom()) -> string().
to_str(Arg) when is_binary(Arg) ->
    unicode:characters_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.

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

-spec info(string()) -> ok.
info(Message) ->
    info(Message, []).

-spec info(string(), [term()]) -> ok.
info(Message, Args) ->
    ColoredMessage = Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

-spec notice(string()) -> ok.
notice(Message) ->
    notice(Message, []).

-spec notice(string(), [term()]) -> ok.
notice(Message, Args) ->
    ColoredMessage = "{{white-bold}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).


-spec error_prn(string()) -> ok.
error_prn(Message) ->
    error_prn(Message, []).

-spec error_prn(string(), [term()]) -> ok.
error_prn(Message, Args) ->
    ColoredMessage = "{{red}}Error: {{reset}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

print(Message, Args) ->
    case application:get_env(elvis, no_output) of
        {ok, true} -> ok;
        _ ->
            Output = io_lib:format(Message, Args),
            EscapedOutput = escape_format_str(Output),
            io:format(parse_colors(EscapedOutput))
    end.


-spec parse_colors(string()) -> string().
parse_colors(Message) ->
    Colors = #{"red" => "\e[0;31m",
               "red-bold" => "\e[1;31m",
               "green" => "\e[0;32m",
               "green-bold" => "\e[1;32m",
               "white" => "\e[0;37m",
               "white-bold" => "\e[1;37m",
               "reset" => "\e[0m"},
    Opts = [global, {return, list}],
    case application:get_env(elvis, output_format, colors) of
        plain ->
            re:replace(Message, "{{.*?}}", "", Opts);
        colors ->
            Fun = fun(Key, Acc) ->
                          Regex = ["{{", Key, "}}"],
                          Color = maps:get(Key, Colors),
                          re:replace(Acc, Regex, Color, Opts)
                  end,
            lists:foldl(Fun, Message, maps:keys(Colors))
    end.

-spec escape_format_str(string()) -> string().
escape_format_str(String) ->
    Binary = list_to_binary(String),
    Result = re:replace(Binary, "[^~]~", "~~", [global]),
    ResultBin = iolist_to_binary(Result),
    binary_to_list(ResultBin).
