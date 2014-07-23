-module(elvis_style).

-export([
         line_length/3,
         no_tabs/3,
         macro_names/3,
         macro_module_names/3,
         operator_spaces/3,
         nesting_level/3,
         god_modules/3,
         no_if_expression/3,
         invalid_dynamic_call/3,
         used_ignored_variable/3,
         no_behavior_info/3,
         module_naming_convention/3,
         exec_path_length/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~p.").
-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").
-define(INVALID_MACRO_NAME_MSG,
            "Invalid macro name ~s on line ~p. Use UPPER_CASE.").
-define(MACRO_AS_MODULE_NAME_MSG,
            "Don't use macros (like ~s on line ~p) as module names.").
-define(MACRO_AS_FUNCTION_NAME_MSG,
            "Don't use macros (like ~s on line ~p) as function names.").
-define(OPERATOR_SPACE_MSG, "Missing space ~s ~p on line ~p").
-define(NESTING_LEVEL_MSG,
        "The expression on line ~p and column ~p is nested "
        "beyond the maximum level of ~p.").
-define(GOD_MODULES_MSG,
        "This module has too many functions (~p). "
        "Considering breaking it into a number of modules.").
-define(NO_IF_EXPRESSION_MSG,
        "Replace the 'if' expression on line ~p with a 'case' "
        "expression or function clauses.").
-define (INVALID_DYNAMIC_CALL_MSG,
         "Remove the dynamic function call on line ~p. "
         "Only modules that define callbacks should make dynamic calls.").
-define(USED_IGNORED_VAR_MSG,
        "Ignored variable is being used on line ~p and "
        "column ~p.").
-define(NO_BEHAVIOR_INFO,
        "Use the '-callback' attribute instead of 'behavior_info/1' "
        "on line ~p.").
-define(MODULE_NAMING_CONVENTION_MSG,
        "The module ~p does not respect the format defined by the "
        "regular expression '~p'.").
-define(EXEC_PATH_LENGTH_MSG,
        "The function ~p is too long, having more than ~p expressions before "
        "it gets to the one on line ~p and column ~p.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(), elvis_utils:file(), [term()]) ->
    [elvis_result:item()].
line_length(_Config, Target, [Limit]) ->
    {Src, _} = elvis_utils:src(Target),
    elvis_utils:check_lines(Src, fun check_line_length/3, [Limit]).

-spec no_tabs(elvis_config:config(), elvis_utils:file(), [term()]) ->
    [elvis_result:item()].
no_tabs(_Config, Target, []) ->
    {Src, _} = elvis_utils:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

-spec macro_names(elvis_config:config(), elvis_utils:file(), [term()]) ->
    [elvis_result:item()].
macro_names(_Config, Target, []) ->
    {Src, _} = elvis_utils:src(Target),
    elvis_utils:check_lines(Src, fun check_macro_names/3, []).

-spec macro_module_names(elvis_config:config(), elvis_utils:file(), []) ->
    [elvis_result:item()].
macro_module_names(_Config, Target, []) ->
    {Src, _} = elvis_utils:src(Target),
    elvis_utils:check_lines(Src, fun check_macro_module_names/3, []).

-spec operator_spaces(elvis_config:config(),
                      elvis_utils:file(),
                      [{right|left, string()}]) ->
    [elvis_result:item()].
operator_spaces(_Config, Target, Rules) ->
    {Src, _} = elvis_utils:src(Target),
    {Root, _} = elvis_utils:parse_tree(Target),
    elvis_utils:check_lines(Src, fun check_operator_spaces/3, {Root, Rules}).

-spec nesting_level(elvis_config:config(), elvis_utils:file(), [integer()]) ->
    [elvis_result:item()].
nesting_level(_Config, Target, [Level]) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

-spec god_modules(elvis_config:config(), elvis_utils:file(), [integer()]) ->
    [elvis_result:item()].
god_modules(_Config, Target, [Limit]) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    Exported = elvis_code:exported_functions(Root),
    case length(Exported) of
        Count when Count > Limit ->
            Msg = ?GOD_MODULES_MSG,
            Result = elvis_result:new(item, Msg, [Count], 1),
            [Result];
        _ -> []
    end.

-spec no_if_expression(elvis_config:config(), elvis_utils:file(), []) ->
    [elvis_result:item()].
no_if_expression(_Config, Target, []) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    Predicate = fun(Node) -> elvis_code:type(Node) == 'if' end,
    ResultFun = result_node_line_fun(?NO_IF_EXPRESSION_MSG),
    case elvis_code:find(Predicate, Root) of
        [] ->
            [];
        IfExprs ->
            lists:map(ResultFun, IfExprs)
    end.

-spec invalid_dynamic_call(elvis_config:config(), elvis_utils:file(), []) ->
    [elvis_result:item()].
invalid_dynamic_call(_Config, Target, IgnoreModules) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    ModuleName = elvis_code:module_name(Root),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            Predicate = fun(Node) -> elvis_code:type(Node) == 'callback' end,
            case elvis_code:find(Predicate, Root) of
                [] ->
                    check_invalid_dynamic_calls(Root);
                _Callbacks ->
                    []
            end;
        true -> []
    end.

-spec used_ignored_variable(elvis_config:config(), elvis_utils:file(), []) ->
    [elvis_result:item()].
used_ignored_variable(_Config, Target, []) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    ResultFun = result_node_line_col_fun(?USED_IGNORED_VAR_MSG),
    case elvis_code:find(fun is_used_ignored_var/1, Root) of
        [] ->
            [];
        UsedIgnoredVars ->
            lists:map(ResultFun, UsedIgnoredVars)
    end.

-spec no_behavior_info(elvis_config:config(), elvis_utils:file(), []) ->
    [elvis_result:item()].
no_behavior_info(_Config, Target, []) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    Children = elvis_code:content(Root),

    FilterFun =
        fun
            (Node) ->
                case elvis_code:type(Node) of
                    function ->
                        Name = elvis_code:attr(name, Node),
                        lists:member(Name,
                                     [behavior_info, behaviour_info]);
                    _ -> false
                end
        end,

    ResultFun = result_node_line_fun(?NO_BEHAVIOR_INFO),

    case lists:filter(FilterFun, Children) of
        [] ->
            [];
        BehaviorInfos ->
            lists:map(ResultFun, BehaviorInfos)
    end.


-spec module_naming_convention(elvis_config:config(),
                               elvis_utils:file(),
                               [list()]) ->
    [elvis_result:item()].
module_naming_convention(_Config, Target, [Regex, IgnoreModules]) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    ModuleName = elvis_code:module_name(Root),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            ModuleNameStr = atom_to_list(ModuleName),
            case re:run(ModuleNameStr, Regex) of
                nomatch ->
                    Msg = ?MODULE_NAMING_CONVENTION_MSG,
                    Info = [ModuleNameStr, Regex],
                    Result = elvis_result:new(item, Msg, Info, 1),
                    [Result];
                {match, _} -> []
            end;
        true ->
            []
    end.

-spec exec_path_length(elvis_config:config(),
                               elvis_utils:file(),
                               [list()]) ->
    [elvis_result:item()].
exec_path_length(_Config, Target, Args = [_MaxCount, IgnoreModules]) ->
    {Root, _} = elvis_utils:parse_tree(Target),
    ModuleName = elvis_code:module_name(Root),
    case lists:member(ModuleName, IgnoreModules) of
        false ->
            elvis_utils:check_nodes(Root, fun check_exec_path_length/2, Args);
        true ->
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Result building

result_node_line_fun(Msg) ->
    fun
        (Node) ->
            {Line, _} = elvis_code:attr(location, Node),
            Info = [Line],
            elvis_result:new(item, Msg, Info, Line)
    end.

result_node_line_col_fun(Msg) ->
    fun
        (Node) ->
            {Line, Col} = elvis_code:attr(location, Node),
            Info = [Line, Col],
            elvis_result:new(item, Msg, Info, Line)
    end.

%% Rule checking

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit]) ->
    case byte_size(Line) of
        Large when Large > Limit ->
            Msg = ?LINE_LENGTH_MSG,
            Info = [Num, binary_to_list(Line)],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result};
        _ ->
            no_result
    end.

-spec check_no_tabs(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_no_tabs(Line, Num, _Args) ->
    case binary:match(Line, <<"\t">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_TABS_MSG,
            Info = [Num, Index],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

-spec check_macro_names(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_macro_names(Line, Num, _Args) ->
    {ok, Regex} = re:compile("^ *[-]define *[(]([^,(]+)"),
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of
        nomatch ->
            no_result;
        {match, [MacroName]} ->
            case string:to_upper(MacroName) of
                MacroName ->
                    no_result;
                _ ->
                    Msg = ?INVALID_MACRO_NAME_MSG,
                    Info = [MacroName, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    {ok, Result}
            end
    end.

-spec check_macro_module_names(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item_result()}.
check_macro_module_names(Line, Num, _Args) ->
    {ok, ModNameRegex} = re:compile("[?]([A-z0-9_]+)[:]"),
    {ok, FunNameRegex} = re:compile("[:][?]([A-z0-9_]+)"),
    case re:run(Line, ModNameRegex, [{capture, all_but_first, list}]) of
        nomatch ->
            case re:run(Line, FunNameRegex, [{capture, all_but_first, list}]) of
                nomatch ->
                    no_result;
                {match, [MacroName]} ->
                    Msg = ?MACRO_AS_FUNCTION_NAME_MSG,
                    Info = [MacroName, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    {ok, Result}
            end;
        {match, [MacroName]} ->
            Msg = ?MACRO_AS_MODULE_NAME_MSG,
            Info = [MacroName, Num],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

-spec check_operator_spaces(binary(), integer(), [{right|left, string()}]) ->
    no_result | {ok, elvis_result:item_result()}.
check_operator_spaces(Line, Num, {Root, Rules}) ->
    AllResults =
        [check_operator_spaces_rule(Line, Num, Rule, Root) || Rule <- Rules],
    case [Result || {ok, Result} <- AllResults] of
        [] -> no_result;
        [Result|_] -> {ok, Result}
    end.
check_operator_spaces_rule(Line, Num, {Position, Operator}, Root) ->
    Escaped = [[$[, Char, $]] || Char <- Operator],
    {Subject, Regex, Label} =
        case Position of
            right ->
                {<<Line/binary, " ">>, Escaped ++ "[^ ]", "after"};
            left ->
                {<<" ", Line/binary>>, "[^ ]" ++ Escaped, "before"}
        end,
    case re:run(Subject, Regex) of
        nomatch ->
            no_result;
        {match, [{Col, _} | _]} ->
            Type = case elvis_code:find_by_location(Root, {Num, Col}) of
                       not_found -> undefined;
                       {ok, Node} -> elvis_code:type(Node)
                   end,
            case Type of
                atom -> [];
                binary_element -> [];
                string -> [];
                _ ->
                    Msg = ?OPERATOR_SPACE_MSG,
                    Info = [Label, Operator, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    {ok, Result}
            end
    end.

-spec check_nesting_level(elvis_code:tree_node(), [integer()]) ->
    [elvis_result:item_result()].
check_nesting_level(ParentNode, [MaxLevel]) ->
    case elvis_code:past_nesting_limit(ParentNode, MaxLevel) of
        [] -> [];
        NestedNodes ->
            Msg = ?NESTING_LEVEL_MSG,

            Fun = fun(Node) ->
                      {Line, Col} = elvis_code:attr(location, Node),
                      Info = [Line, Col, MaxLevel],
                      elvis_result:new(item, Msg, Info, Line)
                  end,

            lists:map(Fun, NestedNodes)
    end.

-spec check_invalid_dynamic_calls(elvis_code:tree_node()) ->
    [elvis_result:item_result()].
check_invalid_dynamic_calls(Root) ->
    case elvis_code:find(fun is_dynamic_call/1, Root) of
        [] -> [];
        InvalidCalls ->
            ResultFun = result_node_line_fun(?INVALID_DYNAMIC_CALL_MSG),
            lists:map(ResultFun, InvalidCalls)
    end.

-spec is_dynamic_call(elvis_code:tree_node()) ->
    boolean().
is_dynamic_call(Node) ->
    case elvis_code:type(Node) of
        call ->
            FunctionSpec = elvis_code:attr(function, Node),
            case elvis_code:type(FunctionSpec) of
                remote ->
                    ModuleName = elvis_code:attr(module, FunctionSpec),
                    var == elvis_code:type(ModuleName);
                _Other ->
                    false
            end;
        _ ->
            false
    end.

-spec is_used_ignored_var(elvis_code:tree_node()) ->
    boolean().
is_used_ignored_var(Node) ->
    case elvis_code:type(Node) of
        var ->
            Name = elvis_code:attr(name, Node),
            [FirstChar | _] = atom_to_list(Name),
            (FirstChar == $_) and (Name =/= '_');
        _OtherType -> false
    end.

-spec check_exec_path_length(elvis_code:tree_node(), [integer()]) ->
    [elvis_result:item_result()].
check_exec_path_length(Node, [MaxLength, _]) ->
    case elvis_code:longest_path(Node) of
        {Length, MaxNode} when Length > MaxLength ->
            {Line, Col} = elvis_code:attr(location, MaxNode),
            Name = elvis_code:attr(name, Node),
            Msg = ?EXEC_PATH_LENGTH_MSG,
            Info = [Name, MaxLength, Line, Col],
            Result = elvis_result:new(item, Msg, Info, Line),
            [Result];
        _Other ->
            []
    end.
