-module(elvis_code).

-export([
         parse_tree/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_tree(string()) -> [{ok | error, erl_parse:abstract_form()}].
parse_tree(Source) ->
    SourceStr = elvis_utils:to_str(Source),
    {ok, Tokens, _} = erl_scan:string(SourceStr, {1, 1}, []),
    {ok, NewTokens} = aleppo:process_tokens(Tokens),

    Forms = split_when(fun is_dot/1, NewTokens),
    [Parsed
     || {ok, Parsed} <- lists:map(fun erl_parse:parse_form/1, Forms)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec split_when(fun(), list()) -> list().
split_when(When, List) ->
    split_when(When, List, [[]]).

split_when(When, [], [[] | Results]) ->
    split_when(When, [], Results);
split_when(_When, [], Results) ->
    Reversed = lists:map(fun lists:reverse/1, Results),
    lists:reverse(Reversed);
split_when(When, [Head | Tail], [Current0 | Rest]) ->
    Current = [Head | Current0],
    Result = case When(Head) of
                 true ->
                     [[], Current | Rest];
                 false ->
                     [Current | Rest]
    end,
    split_when(When, Tail, Result).

is_dot({dot, _}) -> true;
is_dot(_) -> false. 
