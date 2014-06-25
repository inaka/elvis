-module(elvis_rules).

-export([
         line_length/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~p.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis:config(), string(), [term()]) ->
    [elvis_result:result()].
line_length(Config, Target, [Limit]) ->
    {ok, Src} = elvis_utils:src(Config, Target),
    Lines = binary:split(Src, <<"\n">>, [global]),
    {Results, _} = lists:foldl(check_line_length(Limit), {[], 1}, Lines),
    Results.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec check_line_length(integer()) -> fun().
check_line_length(Limit) ->
    fun(Line, {Results, Num}) ->
          case byte_size(Line) of
        Large when Large > Limit ->
                  Msg = ?LINE_LENGTH_MSG,
                  Info = [Num, binary_to_list(Line)],
                  Result = elvis_result:new(specific, Msg, Info),
                  {[Result | Results], Num + 1};
              _ ->
                  {Results, Num + 1}
          end
    end.
