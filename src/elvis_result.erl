-module(elvis_result).

%% API
-export([
         new/3,
         new/4,
         status/1,
         print/1
        ]).

-export([
         get_file/1,
         get_rules/1,
         get_name/1,
         get_items/1,
         get_message/1,
         get_info/1,
         get_line_num/1
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
           file => elvis_file:file(),
           rules => list()
         }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% New

-spec new(item | rule | file, any(), any()) ->
    item() | rule() | file().
new(item, Msg, Info) ->
    #{message => Msg, info => Info};
new(rule, Name, Results) ->
    #{name => Name, items => Results};
new(file, File, Rules) ->
    #{file => File, rules => Rules}.

-spec new(item, term(), any(), any()) -> item().
new(item, Msg, Info, LineNum) ->
    Item = new(item, Msg, Info),
    Item#{line_num => LineNum}.

%% Getters

-spec get_file(file()) -> elvis_file:file().
get_file(#{file := File}) -> File.

-spec get_rules(file()) -> [rule()].
get_rules(#{rules := Rules}) -> Rules.

-spec get_name(rule()) -> atom().
get_name(#{name := Name}) -> Name.

-spec get_items(rule()) -> [item()].
get_items(#{items := Items}) -> Items.

-spec get_message(item()) -> string().
get_message(#{message := Message}) -> Message.

-spec get_info(item()) -> list().
get_info(#{info := Info}) -> Info.

-spec get_line_num(item()) -> list().
get_line_num(#{line_num := LineNum}) -> LineNum.

%% Print

-spec print(item() | rule() | [file()]) -> ok.
print([]) ->
    ok;
print([Result | Results]) ->
    print(Result),
    print(Results);

print(#{file := File, rules := Rules}) ->
    Path = elvis_file:path(File),
    Status = case status(Rules) of
                 ok -> "{{green-bold}}OK";
                 fail -> "{{red-bold}}FAIL"
             end,

    elvis_utils:notice("# ~s [~s{{white-bold}}]", [Path, Status]),
    print(Rules);

print(#{items := []}) ->
    ok;
print(#{name := Name, items := Items}) ->
    elvis_utils:notice("  - ~s", [atom_to_list(Name)]),
    print(Items);

print(#{message := Msg, info := Info}) ->
    elvis_utils:notice("    - " ++ Msg, Info).

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
