-module(elvis_style).

-export([
         function_naming_convention/3,
         variable_naming_convention/3,
         line_length/3,
         no_tabs/3,
         no_spaces/3,
         no_trailing_whitespace/3,
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
         state_record_and_type/3,
         no_spec_with_records/3,
         dont_repeat_yourself/3,
         max_module_length/3,
         max_function_length/3,
         no_debug_call/3,
         no_nested_try_catch/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~s.").

-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").

-define(NO_SPACES_MSG, "Line ~p has a spaces at column ~p.").

-define(NO_TRAILING_WHITESPACE_MSG,
        "Line ~b has ~b trailing whitespace characters.").

-define(INVALID_MACRO_NAME_MSG,
        "Invalid macro name ~s on line ~p. Use UPPER_CASE.").

-define(MACRO_AS_MODULE_NAME_MSG,
        "Don't use macros (like ~s on line ~p) as module names.").
-define(MACRO_MODULE_NAMES_EXCEPTIONS,
        ["MODULE"]).

-define(MACRO_AS_FUNCTION_NAME_MSG,
            "Don't use macros (like ~s on line ~p) as function names.").

-define(OPERATOR_SPACE_MSG, "Missing space ~s ~p on line ~p").

-define(NESTING_LEVEL_MSG,
        "The expression on line ~p and column ~p is nested "
        "beyond the maximum level of ~p.").

-define(GOD_MODULES_MSG,
        "This module has too many functions (~p). "
        "Consider breaking it into a number of modules.").

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

-define(FUNCTION_NAMING_CONVENTION_MSG,
        "The function ~p does not respect the format defined by the "
        "regular expression '~p'.").

-define(VARIABLE_NAMING_CONVENTION_MSG,
        "The variable ~p on line ~p does not respect the format "
        "defined by the regular expression '~p'.").

-define(MODULE_NAMING_CONVENTION_MSG,
        "The module ~p does not respect the format defined by the "
        "regular expression '~p'.").

-define(STATE_RECORD_MISSING_MSG,
        "This module implements an OTP behavior but is missing "
        "a 'state' record.").

-define(STATE_TYPE_MISSING_MSG,
        "This module implements an OTP behavior and has a 'state' record "
        "but is missing a 'state()' type.").

-define(NO_SPEC_WITH_RECORDS,
        "The spec in line ~p uses a record, please define a type for the "
        "record and use that instead.").

-define(DONT_REPEAT_YOURSELF,
        "The code in the following (LINE, COL) locations has "
        "the same structure: ~s.").

-define(MAX_MODULE_LENGTH,
        "The code for module ~p has ~p lines which exceeds the "
        "maximum of ~p.").

-define(MAX_FUNCTION_LENGTH,
        "The code for function ~p has ~p lines which exceeds the "
        "maximum of ~p.").

-define(NO_DEBUG_CALL_MSG,
        "Remove the debug call to ~p:~p/~p on line ~p.").

-define(NO_NESTED_TRY_CATCH,
        "Nested try...catch block starting at line ~p.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type empty_rule_config() :: #{}.

-type function_naming_convention_config() :: #{regex => string(),
                                               ignore => [module()]
                                              }.

-spec function_naming_convention(elvis_config:config(),
                                 elvis_file:file(),
                                 function_naming_convention_config()) ->
    [elvis_result:item()].
function_naming_convention(Config, Target, RuleConfig) ->
    Regex = maps:get(regex, RuleConfig, ".*"),

    {Root, _} = elvis_file:parse_tree(Config, Target),
    FunctionNames = elvis_code:function_names(Root),
    errors_for_function_names(Regex, FunctionNames).

errors_for_function_names(_Regex, []) -> [];
errors_for_function_names(Regex, [FunctionName | RemainingFuncNames]) ->
    FunctionNameStr = atom_to_list(FunctionName),
    case re:run(FunctionNameStr, Regex) of
        nomatch ->
            Msg = ?FUNCTION_NAMING_CONVENTION_MSG,
            Info = [FunctionNameStr, Regex],
            Result = elvis_result:new(item, Msg, Info, 1),
            [Result | errors_for_function_names(Regex, RemainingFuncNames)];
        {match, _} -> errors_for_function_names(Regex, RemainingFuncNames)
    end.

-type variable_naming_convention_config() :: #{regex => string(),
                                               ignore => [module()]
                                              }.
-spec variable_naming_convention(elvis_config:config(),
                                 elvis_file:file(),
                                 variable_naming_convention_config()) ->
    [elvis_result:item()].
variable_naming_convention(Config, Target, RuleConfig) ->
    IgnoreModules = maps:get(ignore, RuleConfig, []),
    Regex = maps:get(regex, RuleConfig, ".*"),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    ModuleName = elvis_code:module_name(Root),
    case lists:member(ModuleName, IgnoreModules) of
        false ->
            IsVar = fun(Node) -> ktn_code:type(Node) =:= 'var' end,
            Vars = elvis_code:find(IsVar, Root, #{traverse => all}),
            check_variables_name(Regex, Vars);
        true -> []
    end.

-type line_length_config() :: #{limit => integer(),
                                skip_comments => false | any | whole_line
                               }.

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(),
                  elvis_file:file(),
                  line_length_config()) ->
    [elvis_result:item()].
line_length(_Config, Target, RuleConfig) ->
    Limit = maps:get(limit, RuleConfig, 80),
    SkipComments = maps:get(skip_comments, RuleConfig, false),
    {Src, _} = elvis_file:src(Target),
    Args = [Limit, SkipComments],
    elvis_utils:check_lines(Src, fun check_line_length/3, Args).

-spec no_tabs(elvis_config:config(),
              elvis_file:file(),
              empty_rule_config()) ->
    [elvis_result:item()].
no_tabs(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

-spec no_spaces(elvis_config:config(),
                elvis_file:file(),
                empty_rule_config()) ->
    [elvis_result:item()].
no_spaces(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_spaces/3, []).

-spec no_trailing_whitespace(elvis_config:config(),
                             elvis_file:file(),
                             map()) ->
    [elvis_result:item()].
no_trailing_whitespace(_Config, Target, RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_trailing_whitespace/3,
                            RuleConfig).

-spec macro_names(elvis_config:config(),
                  elvis_file:file(),
                  empty_rule_config()) ->
    [elvis_result:item()].
macro_names(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_macro_names/3, []).

-spec macro_module_names(elvis_config:config(),
                         elvis_file:file(),
                         empty_rule_config()) ->
    [elvis_result:item()].
macro_module_names(Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    elvis_utils:check_lines(Src, fun check_macro_module_names/3, [Root]).

-type operator_spaces_config() :: #{rules => [{right|left, string()}]}.

-spec operator_spaces(elvis_config:config(),
                      elvis_file:file(),
                      operator_spaces_config()) ->
    [elvis_result:item()].
operator_spaces(Config, Target, RuleConfig) ->
    Rules = maps:get(rules, RuleConfig, []),
    {Src, _} = elvis_file:src(Target),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    elvis_utils:check_lines(Src, fun check_operator_spaces/3, {Root, Rules}).

-type nesting_level_config() :: #{level => integer()}.

-spec nesting_level(elvis_config:config(),
                    elvis_file:file(),
                    nesting_level_config()) ->
    [elvis_result:item()].
nesting_level(Config, Target, RuleConfig) ->
    Level = maps:get(level, RuleConfig, 3),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

-type god_modules_config() :: #{limit => integer()}.

-spec god_modules(elvis_config:config(),
                  elvis_file:file(),
                  god_modules_config()) ->
    [elvis_result:item()].
god_modules(Config, Target, RuleConfig) ->
    Limit = maps:get(limit, RuleConfig, 25),
    IgnoreModules = maps:get(ignore, RuleConfig, []),

    {Root, _} = elvis_file:parse_tree(Config, Target),
    ModuleName = elvis_code:module_name(Root),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            Exported = elvis_code:exported_functions(Root),
            case length(Exported) of
                Count when Count > Limit ->
                    Msg = ?GOD_MODULES_MSG,
                    Result = elvis_result:new(item, Msg, [Count], 1),
                    [Result];
                _ ->
                    []
            end;
        true->
            []
    end.

-spec no_if_expression(elvis_config:config(),
                       elvis_file:file(),
                       empty_rule_config()) ->
    [elvis_result:item()].
no_if_expression(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    Predicate = fun(Node) -> ktn_code:type(Node) == 'if' end,
    ResultFun = result_node_line_fun(?NO_IF_EXPRESSION_MSG),
    case elvis_code:find(Predicate, Root) of
        [] ->
            [];
        IfExprs ->
            lists:map(ResultFun, IfExprs)
    end.

-type invalid_dynamic_call_config() :: #{ignore => [module()]}.

-spec invalid_dynamic_call(elvis_config:config(),
                           elvis_file:file(),
                           invalid_dynamic_call_config()) ->
    [elvis_result:item()].
invalid_dynamic_call(Config, Target, RuleConfig) ->
    IgnoreModules = maps:get(ignore, RuleConfig, []),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    ModuleName = elvis_code:module_name(Root),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            Predicate = fun(Node) -> ktn_code:type(Node) == 'callback' end,
            case elvis_code:find(Predicate, Root) of
                [] ->
                    check_invalid_dynamic_calls(Root);
                _Callbacks ->
                    []
            end;
        true -> []
    end.

-spec used_ignored_variable(elvis_config:config(),
                            elvis_file:file(),
                            empty_rule_config()) ->
    [elvis_result:item()].
used_ignored_variable(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    ResultFun = result_node_line_col_fun(?USED_IGNORED_VAR_MSG),
    case elvis_code:find(fun is_ignored_var/1, Root, #{mode => zipper}) of
        [] ->
            [];
        UsedIgnoredVars ->
            lists:map(ResultFun, UsedIgnoredVars)
    end.

-spec no_behavior_info(elvis_config:config(),
                       elvis_file:file(),
                       empty_rule_config()) ->
    [elvis_result:item()].
no_behavior_info(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    Children = ktn_code:content(Root),

    FilterFun =
        fun
            (Node) ->
                case ktn_code:type(Node) of
                    function ->
                        Name = ktn_code:attr(name, Node),
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

-type module_naming_convention_config() :: #{regex => string(),
                                             ignore => [module()]
                                            }.

-spec module_naming_convention(elvis_config:config(),
                               elvis_file:file(),
                               module_naming_convention_config()) ->
    [elvis_result:item()].
module_naming_convention(Config, Target, RuleConfig) ->
    Regex = maps:get(regex, RuleConfig, ".*"),
    IgnoreModules = maps:get(ignore, RuleConfig, []),

    {Root, _} = elvis_file:parse_tree(Config, Target),
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
        true -> []
    end.

-spec state_record_and_type(elvis_config:config(),
                            elvis_file:file(),
                            empty_rule_config()) ->
    [elvis_result:item()].
state_record_and_type(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    case is_otp_module(Root) of
        true ->
            case {has_state_record(Root), has_state_type(Root)} of
                {true, true} -> [];
                {false, _} ->
                    Msg = ?STATE_RECORD_MISSING_MSG,
                    Result = elvis_result:new(item, Msg, [], 1),
                    [Result];
                {true, false} ->
                    Msg = ?STATE_TYPE_MISSING_MSG,
                    Result = elvis_result:new(item, Msg, [], 1),
                    [Result]
            end;
        false ->
            []
    end.

-spec no_spec_with_records(elvis_config:config(),
                           elvis_file:file(),
                           empty_rule_config()) ->
    [elvis_result:item()].
no_spec_with_records(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    case elvis_code:find(fun spec_includes_record/1, Root) of
        [] -> [];
        SpecNodes ->
            ResultFun = result_node_line_fun(?NO_SPEC_WITH_RECORDS),
            lists:map(ResultFun, SpecNodes)
    end.

-spec dont_repeat_yourself(elvis_config:config(),
                           elvis_file:file(),
                           empty_rule_config()) ->
    [elvis_result:item()].
dont_repeat_yourself(Config, Target, RuleConfig) ->
    MinComplexity = maps:get(min_complexity, RuleConfig, 5),

    {Root, _} = elvis_file:parse_tree(Config, Target),

    case find_repeated_nodes(Root, MinComplexity) of
        [] -> [];
        Nodes ->
            LocationCat =
                fun
                    ({Line, Col}, "") ->
                        io_lib:format("(~p, ~p)", [Line, Col]);
                    ({Line, Col}, Str) ->
                        io_lib:format("~s, (~p, ~p)", [Str, Line, Col])
                end,
            ResultFun =
                fun([{Line, _} | _] = Locations) ->
                        LocationsStr = lists:foldl(LocationCat, "", Locations),
                        Info = [LocationsStr],
                        Msg = ?DONT_REPEAT_YOURSELF,
                        elvis_result:new(item, Msg, Info, Line)
                end,
            lists:map(ResultFun, Nodes)
    end.

-spec max_module_length(elvis_config:config(),
                        elvis_file:file(),
                        empty_rule_config()) ->
    [elvis_result:item()].
max_module_length(Config, Target, RuleConfig) ->
    MaxLength = maps:get(max_length, RuleConfig, 500),
    IgnoreModules = maps:get(ignore, RuleConfig, []),
    CountComments = maps:get(count_comments, RuleConfig, false),
    CountWhitespace = maps:get(count_whitespace, RuleConfig, false),

    {Root, _} = elvis_file:parse_tree(Config, Target),
    {Src, _} = elvis_file:src(Target),


    ModuleName = elvis_code:module_name(Root),
    Ignored = lists:member(ModuleName, IgnoreModules),

    FilterFun =
        fun(Line) ->
                (CountComments orelse (not line_is_comment(Line)))
                    andalso (CountWhitespace
                             orelse (not line_is_whitespace(Line)))
        end,
    Lines = case binary:split(Src, <<"\n">>, [global, trim]) of
                Ls when CountComments andalso CountWhitespace -> Ls;
                Ls -> lists:filter(FilterFun, Ls)
            end,

    case length(Lines) of
        L when L > MaxLength, not Ignored ->
            Info = [ModuleName, L, MaxLength],
            Msg = ?MAX_MODULE_LENGTH,
            Result = elvis_result:new(item, Msg, Info, 0),
            [Result];
        _ ->
            []
    end.

-spec max_function_length(elvis_config:config(),
                          elvis_file:file(),
                          empty_rule_config()) ->
    [elvis_result:item()].
max_function_length(Config, Target, RuleConfig) ->
    MaxLength = maps:get(max_length, RuleConfig, 30),
    CountComments = maps:get(count_comments, RuleConfig, false),
    CountWhitespace = maps:get(count_whitespace, RuleConfig, false),

    {Root, _} = elvis_file:parse_tree(Config, Target),
    {Src, _} = elvis_file:src(Target),
    Lines = binary:split(Src, <<"\n">>, [global, trim]),

    IsFunction = fun(Node) -> ktn_code:type(Node) == function end,
    Functions = elvis_code:find(IsFunction, Root),

    FilterFun =
        fun(Line) ->
                (CountComments orelse (not line_is_comment(Line)))
                    andalso (CountWhitespace
                             orelse (not line_is_whitespace(Line)))
        end,

    PairFun =
        fun(FunctionNode) ->
                Name = ktn_code:attr(name, FunctionNode),
                {Min, Max} = node_line_limits(FunctionNode),
                FunLines = lists:sublist(Lines, Min, Max - Min + 1),
                FilteredLines = lists:filter(FilterFun, FunLines),
                L = length(FilteredLines),
                {Name, Min, L}
        end,

    FunLenInfos = lists:map(PairFun, Functions),
    MaxLengthPred = fun({_, _, L}) -> L > MaxLength end,
    FunLenMaxPairs = lists:filter(MaxLengthPred, FunLenInfos),

    ResultFun =
        fun({Name, StartPos, L}) ->
                Info = [Name, L, MaxLength],
                Msg = ?MAX_FUNCTION_LENGTH,
                elvis_result:new(item, Msg, Info, StartPos)
        end,
    lists:map(ResultFun, FunLenMaxPairs).

-type no_debug_call_config() :: #{debug_functions => [function_spec()],
                                  ignore => [module()]
                                 }.
-type function_spec() :: {module(), atom(), non_neg_integer()}
                       | {module(), atom()}.

-spec no_debug_call(elvis_config:config(),
                    elvis_file:file(),
                    no_debug_call_config()) ->
    [elvis_result:item()].
no_debug_call(Config, Target, RuleConfig) ->
    IgnoreModules = maps:get(ignore, RuleConfig, []),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    ModuleName = elvis_code:module_name(Root),
    DefaultDebugFuns = [{ct, pal},
                        {io, format, 1},
                        {io, format, 2}],
    DebugFuns = maps:get(debug_functions, RuleConfig, DefaultDebugFuns),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            IsCall = fun(Node) -> ktn_code:type(Node) =:= 'call' end,
            Calls = elvis_code:find(IsCall, Root),
            check_no_debug_call(Calls, DebugFuns);
        true -> []
    end.

-spec node_line_limits(ktn_code:tree_node())->
    {Min :: integer(), Max :: integer()}.
node_line_limits(FunctionNode) ->
    Zipper = elvis_code:code_zipper(FunctionNode),
    LineFun = fun(N) -> {L, _} = ktn_code:attr(location, N), L end,
    LineNums = zipper:map(LineFun, Zipper),
    Max = lists:max(LineNums),
    Min = lists:min(LineNums),
    {Min, Max}.

-spec no_nested_try_catch(elvis_config:config(),
                          elvis_file:file(),
                          empty_rule_config()) ->
    [elvis_result:item()].
no_nested_try_catch(Config, Target, _RuleConfig) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    Predicate = fun(Node) -> ktn_code:type(Node) == 'try' end,
    ResultFun = result_node_line_fun(?NO_NESTED_TRY_CATCH),
    case elvis_code:find(Predicate, Root) of
        [] ->
            [];
        TryExprs ->
            lists:flatmap(fun (TryExp) ->
                                   check_nested_try_catchs(ResultFun, TryExp)
                          end,
                          TryExprs)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Variables name
check_variables_name(_Regex, []) -> [];
check_variables_name(Regex, [Variable | RemainingVars]) ->
    VariableName = atom_to_list(ktn_code:attr(name, Variable)),
    %% Replace the leading underline (if any) in the variable name.
    VariableNameStr = case length(VariableName) of
        (N) when N > 1 ->
            [_ | TempStr] = re:replace(VariableName, "^_?", ""),
            binary_to_list(TempStr);
        (_) -> VariableName
    end,
    case re:run(VariableNameStr, Regex) of
        nomatch when VariableNameStr == "_" ->
            check_variables_name(Regex, RemainingVars);
        nomatch ->
            Msg = ?VARIABLE_NAMING_CONVENTION_MSG,
            {Line, _} = ktn_code:attr(location, Variable),
            Info = [VariableNameStr, Line, Regex],
            Result = elvis_result:new(item, Msg, Info, Line),
            [Result | check_variables_name(Regex, RemainingVars)];
        {match, _} -> check_variables_name(Regex, RemainingVars)
    end.

%% Result building

result_node_line_fun(Msg) ->
    fun(Node) ->
            {Line, _} = ktn_code:attr(location, Node),
            Info = [Line],
            elvis_result:new(item, Msg, Info, Line)
    end.

result_node_line_col_fun(Msg) ->
    fun(Node) ->
            {Line, Col} = ktn_code:attr(location, Node),
            Info = [Line, Col],
            elvis_result:new(item, Msg, Info, Line)
    end.

%%% Rule checking

%% Line Length

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re:run(Line, "^[ \t]*%") of
        nomatch    -> false;
        {match, _} -> true
    end.

-spec line_is_whitespace(binary()) -> boolean().
line_is_whitespace(Line) ->
    case re:run(Line, "^[ \t]*$") of
        nomatch    -> false;
        {match, _} -> true
    end.

-spec remove_comment(binary()) -> binary().
remove_comment(Line) ->
    case re:run(Line, "([^%]+)", [{capture, first, binary}]) of
        nomatch            -> Line;
        {match, [Without]} -> Without
    end.

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit, whole_line]) ->
    case line_is_comment(Line) of
        false -> check_line_length(Line, Num, Limit);
        true  -> no_result
    end;
check_line_length(Line, Num, [Limit, any]) ->
    LineWithoutComment = remove_comment(Line),
    check_line_length(LineWithoutComment, Num, Limit);
check_line_length(Line, Num, [Limit|_]) ->
    check_line_length(Line, Num, Limit);
check_line_length(Line, Num, Limit) ->
    Chars = unicode:characters_to_list(Line),
    case length(Chars) of
        Large when Large > Limit ->
            Msg = ?LINE_LENGTH_MSG,
            Info = [Num, binary_to_list(Line)],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result};
        _ ->
            no_result
    end.

%% No Tabs

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

%% No Spaces

-spec check_no_spaces(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_no_spaces(Line, Num, _Args) ->
    case re:run(Line, <<"^\t* ">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_SPACES_MSG,
            Info = [Num, Index],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% No Trailing Whitespace

-spec check_no_trailing_whitespace(binary(), integer(), map()) ->
    no_result | {ok, elvis_result:item()}.
check_no_trailing_whitespace(Line, Num, RuleConfig) ->
    Regex =
        case RuleConfig of
            %% Lookbehind assertion: http://erlang.org/doc/man/re.html#sect17
            #{ignore_empty_lines := true} -> "(?<=\\S)\\s+$";
            _AnythingElse                 -> "\\s+$"
        end,

    case re:run(Line, Regex) of
        nomatch ->
            no_result;
        {match, [PosLen]} ->
            Msg = ?NO_TRAILING_WHITESPACE_MSG,
            Info = [Num, size(binary:part(Line, PosLen))],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% Macro Names

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
                    Result = elvis_result:new(item, Msg, [MacroName, Num], Num),
                    {ok, Result}
            end
    end.

%% Macro in Function Call as Module or Functon Name

-spec check_macro_module_names(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item_result()}.
check_macro_module_names(Line, Num, [Root]) ->
    {ok, ModNameRegex} = re:compile("[?](\\w+)[:][?]?\\w+\\s*\\("),
    {ok, FunNameRegex} = re:compile("[?]?\\w+[:][?](\\w+)\\s*\\("),

    ModuleMsg = ?MACRO_AS_MODULE_NAME_MSG,
    ModuleResults =
        apply_macro_module_names(Line, Num, ModNameRegex, ModuleMsg, Root),

    FunctionMsg = ?MACRO_AS_FUNCTION_NAME_MSG,
    FunResults =
        apply_macro_module_names(Line, Num, FunNameRegex, FunctionMsg, Root),

    case FunResults ++ ModuleResults of
        [] ->
            no_result;
        Results ->
            {ok, Results}
    end.

-spec apply_macro_module_names(binary(),
                               integer(),
                               string(),
                               string(),
                               term()) ->
    [elvis_result:item_result()].
apply_macro_module_names(Line, Num, Regex, Msg, Root) ->
    case re:run(Line, Regex, [{capture, all_but_first, index}]) of
        nomatch ->
            [];
        {match, [{Col, Len}]} ->
            MacroName = binary_to_list(binary:part(Line, Col, Len)),
            case
                lists:member(MacroName, ?MACRO_MODULE_NAMES_EXCEPTIONS)
                orelse not is_remote_call({Num, Col}, Root)
            of
                true ->
                    [];
                false ->
                    Result = elvis_result:new(item, Msg, [MacroName, Num], Num),
                    [Result]
            end
    end.

is_remote_call({Num, Col}, Root) ->
    case elvis_code:find_by_location(Root, {Num, Col}) of
        not_found ->
            true;
        {ok, Node0} ->
            Pred =
                fun(Zipper) ->
                        (Node0 == zipper:node(Zipper))
                            andalso has_remote_call_parent(Zipper)
                end,
            [] =/= elvis_code:find(Pred, Root, #{mode => zipper})
    end.

has_remote_call_parent(undefined) ->
    false;
has_remote_call_parent(Zipper) ->
    Node = zipper:node(Zipper),
    case ktn_code:type(Node) of
        remote ->
            true;
        _ ->
            has_remote_call_parent(zipper:up(Zipper))
    end.

%% Operator Spaces

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
            Type = case elvis_code:find_by_location(Root, {Num, Col + 1}) of
                       not_found -> undefined;
                       {ok, Node} -> ktn_code:type(Node)
                   end,
            TokenType = case elvis_code:find_token(Root, {Num, Col}) of
                            not_found -> undefined;
                            {ok, Token} -> ktn_code:type(Token)
                        end,
            case {Type, TokenType} of
                {atom, _}           -> [];
                {binary_element, _} -> [];
                {string, _}         -> [];
                {char, _}           -> [];
                {comment, _}        -> [];
                {_, string}         -> [];
                _ ->
                    Msg = ?OPERATOR_SPACE_MSG,
                    Info = [Label, Operator, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    {ok, Result}
            end
    end.

%% Nesting Level

-spec check_nesting_level(ktn_code:tree_node(), [integer()]) ->
    [elvis_result:item_result()].
check_nesting_level(ParentNode, [MaxLevel]) ->
    case elvis_code:past_nesting_limit(ParentNode, MaxLevel) of
        [] -> [];
        NestedNodes ->
            Msg = ?NESTING_LEVEL_MSG,

            Fun = fun(Node) ->
                          {Line, Col} = ktn_code:attr(location, Node),
                          Info = [Line, Col, MaxLevel],
                          elvis_result:new(item, Msg, Info, Line)
                  end,

            lists:map(Fun, NestedNodes)
    end.

%% Invalid Dynamic Calls

-spec check_invalid_dynamic_calls(ktn_code:tree_node()) ->
    [elvis_result:item_result()].
check_invalid_dynamic_calls(Root) ->
    case elvis_code:find(fun is_dynamic_call/1, Root) of
        [] -> [];
        InvalidCalls ->
            ResultFun = result_node_line_fun(?INVALID_DYNAMIC_CALL_MSG),
            lists:map(ResultFun, InvalidCalls)
    end.

-spec is_dynamic_call(ktn_code:tree_node()) -> boolean().
is_dynamic_call(Node) ->
    case ktn_code:type(Node) of
        call ->
            FunctionSpec = ktn_code:node_attr(function, Node),
            case ktn_code:type(FunctionSpec) of
                remote ->
                    ModuleName = ktn_code:node_attr(module, FunctionSpec),
                    var == ktn_code:type(ModuleName);
                _Other ->
                    false
            end;
        _ ->
            false
    end.

%% Ignored Variable

-spec is_ignored_var(ktn_code:tree_node()) ->
    boolean().
is_ignored_var(Zipper) ->
    Node = zipper:node(Zipper),
    case ktn_code:type(Node) of
        var ->
            Name = ktn_code:attr(name, Node),
            [FirstChar | _] = atom_to_list(Name),
            (FirstChar == $_)
                and (Name =/= '_')
                and not check_parent_match(Zipper);
        _OtherType -> false
    end.

check_parent_match(Zipper) ->
    case zipper:up(Zipper) of
        undefined -> false;
        ParentZipper ->
            Parent = zipper:node(ParentZipper),
            case ktn_code:type(Parent) of
                match ->
                    zipper:down(ParentZipper) == Zipper;
                _ -> check_parent_match(ParentZipper)
            end
    end.

%% State record in OTP module

-spec is_otp_module(ktn_code:tree_node()) -> boolean().
is_otp_module(Root) ->
    OtpSet = sets:from_list([gen_server,
                             gen_event,
                             gen_fsm,
                             supervisor_bridge
                            ]),
    IsBehaviorAttr = fun(Node) -> behavior == ktn_code:type(Node) end,
    case elvis_code:find(IsBehaviorAttr, Root) of
        [] ->
            false;
        Behaviors ->
            ValueFun = fun(Node) -> ktn_code:attr(value, Node) end,
            Names = lists:map(ValueFun, Behaviors),
            BehaviorsSet = sets:from_list(Names),
            case sets:to_list(sets:intersection(OtpSet, BehaviorsSet)) of
                [] -> false;
                _ -> true
            end
    end.

-spec has_state_record(ktn_code:tree_node()) -> boolean().
has_state_record(Root) ->
    IsStateRecord =
        fun(Node) ->
                (record_attr == ktn_code:type(Node))
                    and (state == ktn_code:attr(name, Node))
        end,
    case elvis_code:find(IsStateRecord, Root) of
        [] -> false;
        _ -> true
    end.

-spec has_state_type(ktn_code:tree_node()) -> boolean().
has_state_type(Root) ->
    IsStateType =
        fun(Node) ->
                (type_attr == ktn_code:type(Node))
                    and (state == ktn_code:attr(name, Node))
        end,
    elvis_code:find(IsStateType, Root) /= [].

%% Spec includes records

-spec spec_includes_record(ktn_code:tree_node()) -> boolean().
spec_includes_record(Node) ->
    IsTypeRecord = fun(Child) ->
                           (ktn_code:type(Child) == type)
                               and (ktn_code:attr(name, Child) == record)
                   end,
    Opts = #{traverse => all},
    (ktn_code:type(Node) == spec)
        and (elvis_code:find(IsTypeRecord, Node, Opts) /= []).

%% Don't repeat yourself

-spec find_repeated_nodes(ktn_code:tree_node(), non_neg_integer()) ->
    [ktn_code:tree_node()].
find_repeated_nodes(Root, MinComplexity) ->
    TypeAttrs = #{var    => [location, name, text],
                  clause => [location, text]},

    FoldFun =
        fun(Node, Map) ->
                Zipper = elvis_code:code_zipper(Node),
                case zipper:size(Zipper) of
                    Count when Count >= MinComplexity ->
                        Loc = ktn_code:attr(location, Node),
                        StrippedNode = remove_attrs_zipper(Zipper, TypeAttrs),

                        ValsSet = maps:get(StrippedNode, Map, sets:new()),
                        NewValsSet = sets:add_element(Loc, ValsSet),
                        maps:put(StrippedNode, NewValsSet, Map);
                    _ ->
                        Map
                end
        end,
    ZipperRoot = elvis_code:code_zipper(Root),
    Grouped = zipper:fold(FoldFun, #{}, ZipperRoot),

    Repeated = filter_repeated(Grouped),
    LocationSets = maps:values(Repeated),
    Locations = lists:map(fun sets:to_list/1, LocationSets),

    lists:map(fun lists:sort/1, Locations).

-spec remove_attrs_zipper(zipper:zipper(), map()) -> ktn_code:tree_node().
remove_attrs_zipper(Zipper, TypeAttrs) ->
    zipper:fmap(fun remove_attrs/2, [TypeAttrs], Zipper).

-spec remove_attrs(ktn_code:tree_node() | [ktn_code:tree_node()], map()) ->
    ktn_code:tree_node().
remove_attrs(Nodes, TypeAttrs) when is_list(Nodes) ->
    [remove_attrs(Node, TypeAttrs) || Node <- Nodes];
remove_attrs(#{attrs := Attrs,
               type := Type,
               node_attrs := NodeAttrs} = Node,
             TypeAttrs) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    NodeAttrsNoLoc =
        [{ Key
         , remove_attrs_zipper(elvis_code:code_zipper(Value),
                               TypeAttrs)}
         || {Key, Value} <- maps:to_list(NodeAttrs)],

    Node#{attrs => AttrsNoLoc,
          node_attrs => maps:from_list(NodeAttrsNoLoc)};
remove_attrs(#{attrs := Attrs, type := Type} = Node, TypeAttrs) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    Node#{attrs => AttrsNoLoc};
remove_attrs(Node, _TypeAttrs) ->
    Node.

-spec filter_repeated(map()) -> map().
filter_repeated(NodesLocs) ->
    NotRepeated = [Node
                   || {Node, LocationSet} <- maps:to_list(NodesLocs),
                      sets:size(LocationSet) == 1],

    RepeatedMap = maps:without(NotRepeated, NodesLocs),

    RepeatedNodes = maps:keys(RepeatedMap),
    Nested = [Node
              || Node <- RepeatedNodes,
                 Parent <- RepeatedNodes,
                 Node =/= Parent,
                 is_children(Parent, Node)],

    maps:without(Nested, RepeatedMap).

is_children(Parent, Node) ->
    Zipper = elvis_code:code_zipper(Parent),
    [] =/= zipper:filter(fun(Child) -> Child == Node end, Zipper).

%% No debug call

-spec check_no_debug_call([ktn_code:node()], [function_spec()]) ->
    [elvis_result:item()].
check_no_debug_call(Calls, DebugFuns) ->
    DebugCalls = [Call || Call <- Calls, is_debug_call(Call, DebugFuns)],
    ResultFun = fun(Call) ->
                        {M, F, A} = call_mfa(Call),
                        {Line, _} = ktn_code:attr(location, Call),
                        elvis_result:new(item,
                                         ?NO_DEBUG_CALL_MSG,
                                         [M, F, A, Line],
                                         Line)
                end,
    lists:map(ResultFun, DebugCalls).

is_debug_call(Call, DebugFuns) ->
    MFA = call_mfa(Call),
    MatchFun = fun(Spec) -> fun_spec_match(Spec, MFA) end,
    lists:any(MatchFun, DebugFuns).

call_mfa(Call) ->
    FunctionSpec = ktn_code:node_attr(function, Call),
    M = ktn_code:attr(value, ktn_code:node_attr(module, FunctionSpec)),
    F = ktn_code:attr(value, ktn_code:node_attr(function, FunctionSpec)),
    A = length(ktn_code:content(Call)),
    {M, F, A}.

fun_spec_match({M, F}, {M, F, _}) -> true;
fun_spec_match({M, F, A}, {M, F, A}) -> true;
fun_spec_match(_, _) -> false.

%% No nested try...catch blocks

check_nested_try_catchs(ResultFun, TryExp) ->
    Predicate = fun(Node) -> ktn_code:type(Node) == 'try' end,
    lists:filtermap(fun (Node) when Node /= TryExp ->
                             {true, ResultFun(Node)};
                        (_) ->
                             false
                    end,
                    elvis_code:find(Predicate, TryExp)).
