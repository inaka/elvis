-module(elvis_code).

%% General
-export([
         parse_tree/1,
         find/2
        ]).

%% Getters
-export([
         type/1,
         attr/2,
         content/1
        ]).


%% Specific
-export([
         past_nesting_limit/2,
         exported_functions/1,
         module_name/1,
         paths_longer_than/2,
         print_node/1,
         print_node/2
        ]).

-type tree_node() ::
        #{type => atom(),
          attrs => map(),
          content => [tree_node()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_tree(string() | binary()) ->
    [{ok | error, erl_parse:abstract_form()}].
parse_tree(Source) ->
    SourceStr = elvis_utils:to_str(Source),
    {ok, Tokens, _} = erl_scan:string(SourceStr, {1, 1}, []),
    {ok, NewTokens} = aleppo:process_tokens(Tokens),

    Forms = split_when(fun is_dot/1, NewTokens),
    ParsedForms = lists:map(fun erl_parse:parse_form/1, Forms),
    Children = [to_map(Parsed) || {ok, Parsed} <- ParsedForms],

    #{type => root,
      content => Children}.

-spec find(fun(), tree_node()) -> [tree_node()].
find(Pred, Node) ->
    Results = find(Pred, Node, []),
    lists:flatten(Results).

find(_Pred, [], Results) ->
    Results;
find(Pred, [Node | Nodes], Results) ->
    [find(Pred, Node, Results) | find(Pred, Nodes, [])];
find(Pred, Node, Results) ->
    Content = elvis_code:content(Node),
    case Pred(Node) of
        true ->
            find(Pred, Content, [Node | Results]);
        false ->
            find(Pred, Content, Results)
    end.

%%% Getters

-spec type(tree_node()) -> atom().
type(#{type := Type}) ->
    Type;
type(undefined) ->
    undefined.

-spec attr(term(), tree_node()) -> term() | undefined.
attr(Key, #{attrs := Attrs}) ->
    case maps:is_key(Key, Attrs) of
        true -> maps:get(Key, Attrs);
        false -> undefined
    end;
attr(_Key, Node) when is_map(Node) ->
    undefined;
attr(_Key, undefined) ->
    undefined.

-spec content(tree_node()) -> [tree_node()].
content(#{content := Content}) ->
    Content;
content(_Node) ->
    [].

%%% Processing functions

%% @doc Takes a node and returns all nodes where the nesting limit is exceeded.
-spec past_nesting_limit(tree_node(), integer()) ->
    [{tree_node(), integer()}].
past_nesting_limit(Node, MaxLevel) ->
    ResultNodes = past_nesting_limit(Node, 1, MaxLevel),
    lists:reverse(ResultNodes).

past_nesting_limit(Node, CurrentLevel, MaxLevel) when CurrentLevel > MaxLevel ->
    [Node];
past_nesting_limit(#{content := Content},
                   CurrentLevel,
                   MaxLevel) ->
    Fun = fun(ChildNode = #{type := Type}) ->
              Increment = level_increment(Type),
              past_nesting_limit(ChildNode,
                                 Increment + CurrentLevel,
                                 MaxLevel)
          end,
    lists:flatmap(Fun, Content);
past_nesting_limit(_Node, _CurrentLeve, _MaxLevel) ->
    [].

%% @doc Debugging utility function.
-spec print_node(tree_node()) -> ok.
print_node(Node) ->
    print_node(Node, 0).

-spec print_node(tree_node(), integer()) -> ok.
print_node(Node = #{type := Type}, CurrentLevel) ->
    Type = type(Node),
    Indentation = lists:duplicate(CurrentLevel * 4, $ ),
    Content = content(Node),
    lager:info("~s - [~p] ~p~n",
               [Indentation, CurrentLevel, Type]),
    lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.

%% @doc Takes the root node and returns the module's name.
-spec module_name(tree_node()) -> atom().
module_name(#{type := root, content := Content}) ->
    Fun = fun (#{type := Type}) -> Type == module end,
    case lists:filter(Fun, Content) of
        [ModuleNode | _] ->
            attr(value, ModuleNode);
        [] -> undefined
    end.

%% @doc Return all the children where the path from the supplied node
%%      is longer than MaxLength.
-spec paths_longer_than(tree_node(), integer()) -> [[tree_node]].
paths_longer_than(Node = #{type := Type}, MaxLength) ->
    Inc = length_increment(Type),
    Result = paths_longer_than(Node, MaxLength, Inc, []),
    lists:flatten(Result).

paths_longer_than(Node, MaxLength, Length, Path)
  when Length > MaxLength ->
    Result = [Node | Path],
    [Result];
paths_longer_than(Node = #{type := Type},
                  MaxLength,
                  Length,
                  Path) ->
    Content = elvis_code:content(Node),
    NewPath = [Node | Path],
    Fun = fun ({Index, Child}) ->
             Inc = length_increment(Type),
             paths_longer_than(Child, MaxLength, Length + Index * Inc + Inc, NewPath)
          end,
    elvis_utils:map_indexed(Fun, Content).


%% @private
%% @doc Takes a node type and determines its nesting level increment.
length_increment(Type) ->
    Increment =
        [
         function,
         'case',
         'if',
         'try',
         'catch',
         'fun',
         named_fun,
         'receive',
         match,
         nil,
         cons,
         map,
         lc,
         var,
         atom,
         integer,
         float,
         string,
         char,
         call
        ],
    case lists:member(Type, Increment) of
        true -> 1;
        false -> 0
    end.

%% @private
%% @doc Takes a node type and determines its nesting level increment.
level_increment(Type) ->
    Increment =
        [
         function,
         'case',
         'if',
         try_case,
         try_catch,
         'fun',
         named_fun,
         receive_case
        ],
    case lists:member(Type, Increment) of
        true -> 1;
        false -> 0
    end.

-spec exported_functions(tree_node()) -> [{atom(), integer()}].
exported_functions(#{type := root, content := Content}) ->
    Fun = fun
              (Node = #{type := export}) ->
                  attr(value, Node);
              (_) -> []
          end,
    lists:flatmap(Fun, Content).

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

-spec is_dot(tuple()) -> boolean().
is_dot({dot, _}) -> true;
is_dot(_) -> false.

%% @doc Converts a parse tree form the abstract format to a map based repr.
%% TODO: Attributes are not being handled correctly.
-spec to_map(term()) -> tree_node().
to_map(ListParsed) when is_list(ListParsed) ->
    lists:map(fun to_map/1, ListParsed);

to_map({function, Location, Name, Arity, Clauses}) ->
    #{type => function,
      attrs => #{location => Location,
                 name => Name,
                 arity => Arity},
      content => to_map(Clauses)};
to_map({function, Name, Arity}) ->
    #{type => function,
      attrs => #{name => Name,
                 arity => Arity}};
to_map({function, Module, Name, Arity}) ->
    #{type => function,
      attrs => #{module => Module,
                 name => Name,
                 arity => Arity}};

to_map({clause, Location, Patterns, Guards, Body}) ->
    #{type => clause,
      attrs => #{location => Location,
                 patterns => to_map(Patterns),
                 guards => to_map(Guards)},
      content => to_map(Body)};

to_map({match, Location, Left, Right}) ->
    #{type => match,
      attrs => #{location => Location},
      content => to_map([Left, Right])};

to_map({tuple, Location, Elements}) ->
    #{type => tuple,
      attrs => #{location => Location},
      content => to_map(Elements)};

%% Literals

to_map({Type, Location, Value}) when
      Type == atom;
      Type == integer;
      Type == float;
      Type == string;
      Type == char ->
    #{type => Type,
      attrs => #{location => Location,
                 value => Value}};

to_map({bin, Location, Elements}) ->
    #{type => binary,
      attrs => #{location => Location},
      content => to_map(Elements)};

to_map({bin_element, Location, Value, Size, TSL}) ->
    #{type => binary_element,
      attrs => #{location => Location,
                 value => to_map(Value),
                 size => Size,
                 type_spec_list => TSL}};

%% Variables

to_map({var, Location, Name}) ->
    #{type => var,
      attrs => #{location => Location,
                 name => Name}};

%% Function call

to_map({call, Location, Function, Arguments}) ->
    #{type => call,
      attrs => #{location => Location,
                 function => to_map(Function),
                 arguments => to_map(Arguments)}};

to_map({remote, Location, Module, Function}) ->
    #{type => remote,
      attrs => #{location => Location,
                 module => to_map(Module),
                 function => to_map(Function)}};

%% case

to_map({'case', Location, Expr, Clauses}) ->
    CaseExpr = to_map({case_expr, Location, Expr}),
    CaseClauses = to_map({case_clauses, Location, Clauses}),
    #{type => 'case',
      attrs => #{location => Location,
                 expression => to_map(Expr)},
      content => [CaseExpr, CaseClauses]};
to_map({case_expr, Location, Expr}) ->
    #{type => case_expr,
      attrs => #{location => Location},
      content => [to_map(Expr)]};
to_map({case_clauses, Location, Clauses}) ->
    #{type => case_clauses,
      attrs => #{location => Location},
      content => to_map(Clauses)};

%% fun

to_map({'fun', Location, {function, Name, Arity}}) ->
    #{type => 'fun',
      attrs => #{location => Location,
                 name => Name,
                 arity => Arity}};

to_map({'fun', Location, {function, Module, Name, Arity}}) ->
    #{type => 'fun',
      attrs => #{location => Location,
                 module => Module,
                 name => Name,
                 arity => Arity}};

to_map({'fun', Location, {clauses, Clauses}}) ->
    #{type => 'fun',
      attrs => #{location => Location},
      content => to_map(Clauses)};

to_map({named_fun, Location, Name, Clauses}) ->
    #{type => named_fun,
      attrs => #{location => Location,
                 name => Name},
      content => to_map(Clauses)};

%% query - deprecated, implemented for completion.

to_map({'query', Location, ListCompr}) ->
    #{type => 'query',
      attrs => #{location => Location},
      content => to_map(ListCompr)};

%% try..catch..after

to_map({'try', Location, Body, [], CatchClauses, AfterBody}) ->
    TryBody = to_map(Body),
    TryCatch = to_map({try_catch, Location, CatchClauses}),
    TryAfter = to_map({try_after, Location, AfterBody}),

    #{type => 'try',
      attrs => #{location => Location,
                 catch_clauses => to_map(CatchClauses),
                 after_body => to_map(AfterBody)},
      content => TryBody ++ [TryCatch, TryAfter]};

%% try..of..catch..after

to_map({'try', Location, Expr, CaseClauses, CatchClauses, AfterBody}) ->
    TryCase = to_map({try_case, Location, Expr, CaseClauses}),
    TryCatch = to_map({try_catch, Location, CatchClauses}),
    TryAfter = to_map({try_after, Location, AfterBody}),

    #{type => 'try',
      attrs => #{location => Location},
      content => [TryCase, TryCatch, TryAfter]};

to_map({try_case, Location, Expr, Clauses}) ->
    #{type => try_case,
      attrs => #{location => Location,
                 expression => Expr},
      content => to_map(Clauses)};

to_map({try_catch, Location, Clauses}) ->
    #{type => try_catch,
      attrs => #{location => Location},
      content => to_map(Clauses)};

to_map({try_after, Location, AfterBody}) ->
    #{type => try_after,
      attrs => #{location => Location},
      content => to_map(AfterBody)};

%% if

to_map({'if', Location, IfClauses}) ->
    #{type => 'if',
      attrs => #{location => Location},
      content => to_map(IfClauses)};

%% catch

to_map({'catch', Location, Expr}) ->
    #{type => 'catch',
      attrs => #{location => Location},
      content => [to_map(Expr)]};

%% receive

to_map({'receive', Location, Clauses}) ->
    RecClauses = to_map({receive_case, Location, Clauses}),
    #{type => 'receive',
      attrs => #{location => Location},
      content => [RecClauses]};

to_map({'receive', Location, Clauses, AfterExpr, AfterBody}) ->
    RecClauses = to_map({receive_case, Location, Clauses}),
    RecAfter = to_map({receive_after, Location, AfterExpr, AfterBody}),
    #{type => 'receive',
      attrs => #{location => Location},
      content => [RecClauses, RecAfter]};

to_map({receive_case, Location, Clauses}) ->
    #{type => receive_case,
      attrs => #{location => Location},
      content => to_map(Clauses)};

to_map({receive_after, Location, Expr, Body}) ->
    #{type => receive_after,
      attrs => #{location => Location,
                 expression => to_map(Expr)},
      content => to_map(Body)};

%% List

to_map({nil, Location}) ->
    #{type => nil,
      attrs => #{location => Location}};

to_map({cons, Location, Head, Tail}) ->
    #{type => cons,
      attrs => #{location => Location,
                 head => to_map(Head),
                 tail => to_map(Tail)}};

%% Map

to_map({map, Location, Pairs}) ->
    #{type => map,
      attrs => #{location => Location},
      content => to_map(Pairs)};
to_map({map, Location, Var, Pairs}) ->
    #{type => map,
      attrs => #{location => Location,
                 var => to_map(Var)},
      content => to_map(Pairs)};

to_map({Type, Location, Key, Value}) when
      map_field_exact == Type;
      map_field_assoc == Type ->
    #{type => map_field_exact,
      attrs => #{location => Location,
                 key => to_map(Key),
                 value => to_map(Value)}};

%% List Comprehension

to_map({lc, Location, Expr, GeneratorsFilters}) ->
    LcExpr = to_map({lc_expr, Location, Expr}),
    LcGenerators = to_map(GeneratorsFilters),
    #{type => lc,
      attrs => #{location => Location},
      content => [LcExpr | LcGenerators]};

to_map({generate, Location, Pattern, Expr}) ->
    #{type => generate,
      attrs => #{location => Location,
                 pattern => to_map(Pattern),
                 expression => to_map(Expr)}};
to_map({lc_expr, Location, Expr}) ->
    #{type => lc_expr,
      attrs => #{location => Location},
      content => [to_map(Expr)]};

%% Operation

to_map({op, Location, Operation, Left, Right}) ->
    #{type => op,
      attrs => #{location => Location,
                 operation => Operation},
      content => to_map([Left, Right])};

to_map({op, Location, Operation, Single}) ->
    #{type => op,
      attrs => #{location => Location,
                 operation => Operation},
      content => to_map([Single])};

%% Record

to_map({record, Location, Name, Fields}) ->
    #{type => record,
      attrs => #{location => Location,
                 name => Name},
      content => to_map(Fields)};
to_map({record, Location, Var, Name, Fields}) ->
    #{type => record,
      attrs => #{location => Location,
                 variable => to_map(Var),
                 name => Name},
      content => to_map(Fields)};

to_map({record_index, Location, Name, Field}) ->
    #{type => record_index,
      attrs => #{location => Location,
                 name => Name},
      content => [to_map(Field)]};

to_map({record_field, Location, Name}) ->
    #{type => record_field,
      attrs => #{location => Location,
                 name => to_map(Name)}};
to_map({record_field, Location, Name, Default}) ->
    #{type => record_field,
      attrs => #{location => Location,
                 default => to_map(Default),
                 name => to_map(Name)}};
to_map({record_field, Location, Var, Name, Field}) ->
    #{type => record_field,
      attrs => #{location => Location,
                 variable => to_map(Var),
                 name => Name},
      content => [to_map(Field)]};

%% Attributes

to_map({attribute, Location, Type, Value}) ->
    #{type => Type,
      attrs => #{location => Location,
                 value => Value}};

%% Unhandled forms

to_map(Parsed) when is_tuple(Parsed) ->
    throw({unhandled_abstract_form, Parsed});
to_map(Parsed) ->
    throw({unexpected_abstract_form, Parsed}).
