-module(elvis_style).

-export([
         line_length/3,
         no_tabs/3,
         macro_names/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~p.").
-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").
-define(INVALID_MACRO_NAME_MSG,
            "Invalid macro name ~s on line ~p. Use UPPER_CASE.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(), map(), [term()]) ->
    [elvis_result:item()].
line_length(Config, Target, [Limit]) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    elvis_utils:check_lines(Src, fun check_line_length/3, [Limit]).

-spec no_tabs(elvis_config:config(), map(), [term()]) ->
    [elvis_result:item()].
no_tabs(Config, Target, []) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

-spec macro_names(elvis_config:config(), map(), [term()]) ->
    [elvis_result:item()].
macro_names(Config, Target, []) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    elvis_utils:check_lines(Src, fun check_macro_names/3, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit]) ->
    case byte_size(Line) of
        Large when Large > Limit ->
            Msg = ?LINE_LENGTH_MSG,
            Info = [Num, binary_to_list(Line)],
            Result = #{message => Msg, info => Info, line_num => Num},
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
            Result = #{message => Msg, info => Info, line_num => Num},
            {ok, Result}
    end.

-spec check_macro_names(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_macro_names(Line, Num, _Args) ->
    {ok, Regex} = re:compile("^ *[-]define *[(]([^,]+)"),
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
                    Result = #{message => Msg, info => Info, line_num => Num},
                    {ok, Result}
            end
    end.
