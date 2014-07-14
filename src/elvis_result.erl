-module(elvis_result).

%% API
-export([
         status/1,
         print/1
        ]).

%% Types
-export_type([
              item/0,
              rule/0,
              file/0
             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type item() ::
        #{
           message => string(),
           info => list()
         }.
-type rule() ::
        #{
           name => atom(),
           items => [item()]
         }.
-type file() ::
        #{
           file => elvis_utils:file(),
           rules => list()
         }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Print

-spec print(item() | rule() | [file()]) -> ok.
print([]) ->
    ok;
print([Result | Results]) ->
    print(Result),
    print(Results);

print(#{file := File, rules := Rules}) ->
    Path = maps:get(path, File),
    Status = case status(Rules) of
                 ok -> "OK";
                 fail -> "FAIL"
             end,

    io:format("# ~s [~s]~n", [Path, Status]),
    print(Rules);

print(#{items := []}) ->
    ok;
print(#{name := Name, items := Items}) ->
    io:format("  - ~s~n", [atom_to_list(Name)]),
    print(Items);

print(#{message := Msg, info := Info}) ->
    io:format("    - " ++ Msg ++ "~n", Info).

-spec status([rule()]) -> ok | fail.
status([]) ->
    ok;
status([#{rules := Rules} | Files]) ->
    case status(Rules) of
        fail -> fail;
        ok -> status(Files)
    end;
status([#{items := []} | Rules]) ->
    status(Rules);
status(_Rules) ->
    fail.
