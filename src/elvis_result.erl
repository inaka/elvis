-module(elvis_result).

-export([
         new/3,
         print/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(result,
        {
          message = "" :: string(),
          info = [] :: list()
        }).

-record(rule_result,
        {
          name :: atom(),
          results = [] :: [result()]
        }).

-record(file_result,
        {
          path = "" :: string(),
          rules = [] :: list()
        }).

-type result() :: #result{}.
-type rule_result() :: #rule_result{}.
-type file_result() :: #file_result{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(specific | rule | file, any(), any()) ->
    result() | rule_result() | file_result().
new(specific, Msg, Info) ->
    #result{message = Msg, info= Info};
new(rule, Name, Results) ->
    #rule_result{name = Name, results = Results};
new(file, Path, Rules) ->
    #file_result{path = Path, rules = Rules}.

-spec print(result() | rule_result() | [file_result()]) -> ok.
print([]) ->
    ok;
print([Result | Results]) ->
    print(Result),
    print(Results);

print(#file_result{rules = []}) ->
    ok;
print(#file_result{path = Path, rules = Rules}) ->
    io:format("# ~s~n", [Path]),
    print(Rules),
    ok;

print(#rule_result{results = []}) ->
    ok;
print(#rule_result{name = Name, results = Results}) ->
    io:format("  - ~s~n", [atom_to_list(Name)]),
    print(Results),
    ok;

print(#result{message = Msg, info = Info}) ->
    io:format("    - " ++ Msg ++ "~n", Info),
    ok.
