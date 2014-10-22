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
         state_record_and_type/3,
         no_spec_with_records/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~p.").

-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(), elvis_file:file(), [term()]) ->
    [elvis_result:item()].
line_length(_Config, Target, [Limit]) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_line_length/3, [Limit]).

-spec no_tabs(elvis_config:config(), elvis_file:file(), [term()]) ->
    [elvis_result:item()].
no_tabs(_Config, Target, []) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

-spec macro_names(elvis_config:config(), elvis_file:file(), [term()]) ->
    [elvis_result:item()].
macro_names(_Config, Target, []) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_macro_names/3, []).

-spec macro_module_names(elvis_config:config(), elvis_file:file(), []) ->
    [elvis_result:item()].
macro_module_names(_Config, Target, []) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_macro_module_names/3, []).

-spec operator_spaces(elvis_config:config(),
                      elvis_file:file(),
                      [{right|left, string()}]) ->
    [elvis_result:item()].
operator_spaces(Config, Target, Rules) ->
    {Src, _} = elvis_file:src(Target),
    {Root, _} = elvis_file:parse_tree(Config, Target),
    elvis_utils:check_lines(Src, fun check_operator_spaces/3, {Root, Rules}).

-spec nesting_level(elvis_config:config(), elvis_file:file(), [integer()]) ->
    [elvis_result:item()].
nesting_level(Config, Target, [Level]) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

-spec god_modules(elvis_config:config(), elvis_file:file(), [integer()]) ->
    [elvis_result:item()].
god_modules(Config, Target, [Limit]) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    Exported = elvis_code:exported_functions(Root),
    case length(Exported) of
        Count when Count > Limit ->
            Msg = ?GOD_MODULES_MSG,
            Result = elvis_result:new(item, Msg, [Count], 1),
            [Result];
        _ -> []
    end.

-spec no_if_expression(elvis_config:config(), elvis_file:file(), []) ->
    [elvis_result:item()].
no_if_expression(Config, Target, []) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    Predicate = fun(Node) -> ktn_code:type(Node) == 'if' end,
    ResultFun = result_node_line_fun(?NO_IF_EXPRESSION_MSG),
    case elvis_code:find(Predicate, Root) of
        [] ->
            [];
        IfExprs ->
            lists:map(ResultFun, IfExprs)
    end.

-spec invalid_dynamic_call(elvis_config:config(), elvis_file:file(), []) ->
    [elvis_result:item()].
invalid_dynamic_call(Config, Target, IgnoreModules) ->
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

-spec used_ignored_variable(elvis_config:config(), elvis_file:file(), []) ->
    [elvis_result:item()].
used_ignored_variable(Config, Target, []) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    ResultFun = result_node_line_col_fun(?USED_IGNORED_VAR_MSG),
    case elvis_code:find_zipper(fun is_ignored_var/1, Root) of
        [] ->
            [];
        UsedIgnoredVars ->
            lists:map(ResultFun, UsedIgnoredVars)
    end.

-spec no_behavior_info(elvis_config:config(), elvis_file:file(), []) ->
    [elvis_result:item()].
no_behavior_info(Config, Target, []) ->
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


-spec module_naming_convention(elvis_config:config(),
                               elvis_file:file(),
                               [list()]) ->
    [elvis_result:item()].
module_naming_convention(Config, Target, [Regex, IgnoreModules]) ->
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
                            [list()]) ->
    [elvis_result:item()].
state_record_and_type(Config, Target, []) ->
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
                           [list()]) ->
    [elvis_result:item()].
no_spec_with_records(Config, Target, []) ->
    {Root, _} = elvis_file:parse_tree(Config, Target),
    case elvis_code:find(fun spec_includes_record/1, Root) of
        [] -> [];
        SpecNodes ->
            ResultFun = result_node_line_fun(?NO_SPEC_WITH_RECORDS),
            lists:map(ResultFun, SpecNodes)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Result building

result_node_line_fun(Msg) ->
    fun
        (Node) ->
            {Line, _} = ktn_code:attr(location, Node),
            Info = [Line],
            elvis_result:new(item, Msg, Info, Line)
    end.

result_node_line_col_fun(Msg) ->
    fun
        (Node) ->
            {Line, Col} = ktn_code:attr(location, Node),
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

    ModuleMsg = ?MACRO_AS_MODULE_NAME_MSG,
    ModuleResults =
        apply_macro_module_names(Line, Num, ModNameRegex, ModuleMsg),

    FunctionMsg = ?MACRO_AS_FUNCTION_NAME_MSG,
    FunResults =
        apply_macro_module_names(Line, Num, FunNameRegex, FunctionMsg),

    case FunResults ++ ModuleResults of
        [] ->
            no_result;
        Results ->
            {ok, Results}
    end.

-spec apply_macro_module_names(binary(), integer(), string(), string()) ->
    [elvis_result:item_result()].
apply_macro_module_names(Line, Num, Regex, Msg) ->
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of
        nomatch ->
            [];
        {match, [MacroName]} ->
            case lists:member(MacroName, ?MACRO_MODULE_NAMES_EXCEPTIONS) of
                    true ->
                    [];
                false ->
                    Info = [MacroName, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    [Result]
            end
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
                       {ok, Node} -> ktn_code:type(Node)
                   end,
            case Type of
                atom -> [];
                binary_element -> [];
                string -> [];
                comment -> [];
                _ ->
                    Msg = ?OPERATOR_SPACE_MSG,
                    Info = [Label, Operator, Num],
                    Result = elvis_result:new(item, Msg, Info, Num),
                    {ok, Result}
            end
    end.

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

-spec check_invalid_dynamic_calls(ktn_code:tree_node()) ->
    [elvis_result:item_result()].
check_invalid_dynamic_calls(Root) ->
    case elvis_code:find(fun is_dynamic_call/1, Root) of
        [] -> [];
        InvalidCalls ->
            ResultFun = result_node_line_fun(?INVALID_DYNAMIC_CALL_MSG),
            lists:map(ResultFun, InvalidCalls)
    end.

-spec is_dynamic_call(ktn_code:tree_node()) ->
    boolean().
is_dynamic_call(Node) ->
    case ktn_code:type(Node) of
        call ->
            FunctionSpec = ktn_code:attr(function, Node),
            case ktn_code:type(FunctionSpec) of
                remote ->
                    ModuleName = ktn_code:attr(module, FunctionSpec),
                    var == ktn_code:type(ModuleName);
                _Other ->
                    false
            end;
        _ ->
            false
    end.

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

-spec is_otp_module(ktn_code:tree_node()) -> boolean().
is_otp_module(Root) ->
    OtpSet = sets:from_list([gen_server,
                             gen_event,
                             gen_fsm,
                             supervisor_bridge
                            ]),
    IsBehaviorAttr = fun(Node) ->  behavior == ktn_code:type(Node) end,
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

-spec spec_includes_record(ktn_code:tree_node()) -> boolean().
spec_includes_record(Node) ->
    IsTypeRecord = fun(Child) ->
                           (ktn_code:type(Child) == type)
                               and (ktn_code:attr(name, Child) == record)
                   end,

    (ktn_code:type(Node) == spec)
        and (elvis_code:find(IsTypeRecord, Node) /= []).
