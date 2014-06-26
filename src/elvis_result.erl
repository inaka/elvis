-module(elvis_result).

%% API
-export([
         new/3,
         print/1
        ]).

%% Types
-export_type([
              item_result/0,
              rule_result/0,
              file_result/0
             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(item_result,
        {
          message = "" :: string(),
          info = [] :: list()
        }).

-record(rule_result,
        {
          name :: atom(),
          results = [] :: [item_result()]
        }).

-record(file_result,
        {
          path = "" :: string(),
          rules = [] :: list()
        }).

-type item_result() :: #item_result{}.
-type rule_result() :: #rule_result{}.
-type file_result() :: #file_result{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(item | rule | file, any(), any()) ->
    item_result() | rule_result() | file_result().
new(item, Msg, Info) ->
    #item_result{message = Msg, info= Info};
new(rule, Name, Results) ->
    #rule_result{name = Name, results = Results};
new(file, Path, Rules) ->
    #file_result{path = Path, rules = Rules}.

-spec print(item_result() | rule_result() | [file_result()]) -> ok.
print([]) ->
    ok;
print([Result | Results]) ->
    print(Result),
    print(Results);

print(#file_result{path = Path, rules = Rules}) ->
    Status = case file_status(Rules) of
                 ok -> "OK";
                 fail -> "FAIL"
             end,

    io:format("# ~s [~s]~n", [Path, Status]),
    print(Rules);

print(#rule_result{results = []}) ->
    ok;
print(#rule_result{name = Name, results = Results}) ->
    io:format("  - ~s~n", [atom_to_list(Name)]),
    print(Results);

print(#item_result{message = Msg, info = Info}) ->
    io:format("    - " ++ Msg ++ "~n", Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec file_status([rule_result()]) -> ok | fail.
file_status([]) ->
    ok;
file_status([#rule_result{results = []} | Rules]) ->
    file_status(Rules);
file_status(_Rules) ->
    fail.
