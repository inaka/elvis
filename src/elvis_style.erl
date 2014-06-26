-module(elvis_style).

-export([
         line_length/3,
         no_tabs/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~p.").
-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis:config(), string(), [term()]) ->
    [elvis_result:item_result()].
line_length(Config, Target, [Limit]) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    elvis_utils:check_lines(Src, fun check_line_length/3, [Limit]).

-spec no_tabs(elvis:config(), string(), [term()]) ->
    [elvis_result:item_result()].
no_tabs(Config, Target, []) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item_result()}.
check_line_length(Line, Num, [Limit]) ->
    case byte_size(Line) of
        Large when Large > Limit ->
            Msg = ?LINE_LENGTH_MSG,
            Info = [Num, binary_to_list(Line)],
            Result = elvis_result:new(item, Msg, Info),
            {ok, Result};
        _ ->
            no_result
    end.

-spec check_no_tabs(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item_result()}.
check_no_tabs(Line, Num, _Args) ->
    case binary:match(Line, <<"\t">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_TABS_MSG,
            Result = elvis_result:new(item, Msg, [Num, Index]),
            {ok, Result}
    end.
