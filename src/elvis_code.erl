-module(elvis_code).

%% General
-export([
         parse_tree/1,
         find/2,
         find_by_location/2
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
         longest_path/1,
         print_node/1,
         print_node/2
        ]).

-type tree_node_type() ::
        function | clause | match | tuple
      | atom | integer | float | string | char
      | binary | binary_element | var
      | call | remote
      | 'case' | case_expr | case_clauses
      | 'fun' | named_fun
      | 'query'
      | 'try' | try_catch | try_case | try_after
      | 'if' | 'catch'
      | 'receive' | receive_after | receive_case
      | nil | cons
      | map | map_field_assoc | map_field_exact
      | lc | lc_expr | generate
      | op
      | record | record_field | record_index
      | block
        %% Attributes
      | module
      | type | callback
      | export | export_type.

-type tree_node() ::
    #{type => tree_node_type(),
      attrs => map(),
      content => [tree_node()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parses code in a string or binary format and returns the parse tree.
-spec parse_tree(string() | binary()) ->
    [{ok | error, erl_parse:abstract_form()}].
parse_tree(Source) ->
    SourceStr = elvis_utils:to_str(Source),
    {ok, Tokens, _} = erl_scan:string(SourceStr, {1, 1}, [text]),
    {ok, NewTokens} = aleppo:process_tokens(Tokens),

    Forms = split_when(fun is_dot/1, NewTokens),
    ParsedForms = lists:map(fun erl_parse:parse_form/1, Forms),
    Children = [to_map(Parsed) || {ok, Parsed} <- ParsedForms],

    #{type => root,
      attrs => #{},
      content => Children}.

%% @doc Finds all nodes that comply with the predicate function.
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

-spec find_by_location(tree_node(), {integer(), integer()}) ->
    not_found | {ok, tree_node()}.
find_by_location(Root, {Line, Column}) ->
    Fun = fun
              (Node = #{attrs := #{location := {NodeLine, NodeCol}}})
                when NodeLine == Line->
                  Text = attr(text, Node),
                  Length = length(Text),
                  (NodeCol =< Column) and (Column =< NodeCol + Length);
              (_) ->
                  false
          end,
    case find(Fun, Root) of
        [] -> not_found;
        [Node] ->
            {ok, Node}
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
-spec longest_path(tree_node()) -> {integer(), tree_node()}.
longest_path(Node) ->
    longest_path(Node, 0).

longest_path(Node = #{type := Type}, Length) ->
    {Line, _} = attr(location, Node),
    NewLength = Length + length_increment(Type),
    %% lager:info("Line: ~p\t~p\t~p", [Line, NewLength, Type]),
    Content = elvis_code:content(Node),
    Fun = fun longest_path_foldl/2,
    Acc = {NewLength, {NewLength, Node}},
    {_, Result} = lists:foldl(Fun, Acc, Content),
    Result.

-spec longest_path_foldl([tree_node()],
                         {integer(), {integer(), tree_node()}}) ->
     [{integer(), tree_node()}].
longest_path_foldl(Node = #{type := Type},
                   {Length,  CurrentMax = {MaxLength, _}}) ->
    ChildMax = longest_path(Node, Length),
    {ChildMaxLength, _} = ChildMax,
    NewLength =
        case is_branch(Type) of
            true -> Length;
            false -> ChildMaxLength
        end,
    NewMax =
        case ChildMaxLength > MaxLength of
            true -> ChildMax;
            false -> CurrentMax
        end,
    {NewLength, NewMax}.

%% @private
is_branch(Type) ->
    Branch = [clause,
              case_clauses,
              catch_clauses,
              recieve_case,
              case_expr,
              try_case,
              match,
              op
             ],
    lists:member(Type, Branch).

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
         nil,
         cons,
         map,
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

%% @private
get_location(Attrs) when is_list(Attrs) ->
    Line = proplists:get_value(line, Attrs),
    Column = proplists:get_value(column, Attrs),
    {Line, Column};
get_location(_Attrs) ->
    {-1, -1}.

%% @private
get_text(Attrs) when is_list(Attrs) ->
    proplists:get_value(text, Attrs, "");
get_text(_Attrs) ->
    "".

%% @doc Converts a parse tree form the abstract format to a map based repr.
%% TODO: Attributes are not being handled correctly.
-spec to_map(term()) -> tree_node().
to_map(ListParsed) when is_list(ListParsed) ->
    lists:map(fun to_map/1, ListParsed);

to_map({function, Attrs, Name, Arity, Clauses}) ->
    #{type => function,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
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

to_map({clause, Attrs, Patterns, Guards, Body}) ->
    #{type => clause,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 patterns => to_map(Patterns),
                 guards => to_map(Guards)},
      content => to_map(Body)};

to_map({match, Attrs, Left, Right}) ->
    #{type => match,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map([Left, Right])};

%% Literals

to_map({Type, Attrs, Value}) when
      Type == atom;
      Type == integer;
      Type == float;
      Type == string;
      Type == char ->
    #{type => Type,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 value => Value}};

to_map({bin, Attrs, Elements}) ->
    #{type => binary,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Elements)};

to_map({bin_element, Attrs, Value, Size, TSL}) ->
    #{type => binary_element,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 value => to_map(Value),
                 size => Size,
                 type_spec_list => TSL}};

%% Variables

to_map({var, Attrs, Name}) ->
    #{type => var,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => Name}};

%% Function call

to_map({call, Attrs, Function, Arguments}) ->
    #{type => call,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 function => to_map(Function)},
      content => to_map(Arguments)};

to_map({remote, Attrs, Module, Function}) ->
    #{type => remote,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 module => to_map(Module),
                 function => to_map(Function)}};

%% case

to_map({'case', Attrs, Expr, Clauses}) ->
    CaseExpr = to_map({case_expr, Attrs, Expr}),
    CaseClauses = to_map({case_clauses, Attrs, Clauses}),
    #{type => 'case',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 expression => to_map(Expr)},
      content => [CaseExpr, CaseClauses]};
to_map({case_expr, Attrs, Expr}) ->
    #{type => case_expr,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [to_map(Expr)]};
to_map({case_clauses, Attrs, Clauses}) ->
    #{type => case_clauses,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Clauses)};

%% fun

to_map({'fun', Attrs, {function, Name, Arity}}) ->
    #{type => 'fun',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => Name,
                 arity => Arity}};

to_map({'fun', Attrs, {function, Module, Name, Arity}}) ->
    #{type => 'fun',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 module => Module,
                 name => Name,
                 arity => Arity}};

to_map({'fun', Attrs, {clauses, Clauses}}) ->
    #{type => 'fun',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Clauses)};

to_map({named_fun, Attrs, Name, Clauses}) ->
    #{type => named_fun,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => Name},
      content => to_map(Clauses)};

%% query - deprecated, implemented for completion.

to_map({'query', Attrs, ListCompr}) ->
    #{type => 'query',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(ListCompr)};

%% try..catch..after

to_map({'try', Attrs, Body, [], CatchClauses, AfterBody}) ->
    TryBody = to_map(Body),
    TryCatch = to_map({try_catch, Attrs, CatchClauses}),
    TryAfter = to_map({try_after, Attrs, AfterBody}),

    #{type => 'try',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 catch_clauses => to_map(CatchClauses),
                 after_body => to_map(AfterBody)},
      content => TryBody ++ [TryCatch, TryAfter]};

%% try..of..catch..after

to_map({'try', Attrs, Expr, CaseClauses, CatchClauses, AfterBody}) ->
    TryCase = to_map({try_case, Attrs, Expr, CaseClauses}),
    TryCatch = to_map({try_catch, Attrs, CatchClauses}),
    TryAfter = to_map({try_after, Attrs, AfterBody}),

    #{type => 'try',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [TryCase, TryCatch, TryAfter]};

to_map({try_case, Attrs, Expr, Clauses}) ->
    #{type => try_case,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 expression => Expr},
      content => to_map(Clauses)};

to_map({try_catch, Attrs, Clauses}) ->
    #{type => try_catch,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Clauses)};

to_map({try_after, Attrs, AfterBody}) ->
    #{type => try_after,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(AfterBody)};

%% if

to_map({'if', Attrs, IfClauses}) ->
    #{type => 'if',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(IfClauses)};

%% catch

to_map({'catch', Attrs, Expr}) ->
    #{type => 'catch',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [to_map(Expr)]};

%% receive

to_map({'receive', Attrs, Clauses}) ->
    RecClauses = to_map({receive_case, Attrs, Clauses}),
    #{type => 'receive',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [RecClauses]};

to_map({'receive', Attrs, Clauses, AfterExpr, AfterBody}) ->
    RecClauses = to_map({receive_case, Attrs, Clauses}),
    RecAfter = to_map({receive_after, Attrs, AfterExpr, AfterBody}),
    #{type => 'receive',
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [RecClauses, RecAfter]};

to_map({receive_case, Attrs, Clauses}) ->
    #{type => receive_case,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Clauses)};

to_map({receive_after, Attrs, Expr, Body}) ->
    #{type => receive_after,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 expression => to_map(Expr)},
      content => to_map(Body)};

%% List

to_map({nil, Attrs}) ->
    #{type => nil,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)}};

to_map({cons, Attrs, Head, Tail}) ->
    #{type => cons,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 head => to_map(Head),
                 tail => to_map(Tail)}};

%% Tuple

to_map({tuple, Location, Elements}) ->
    #{type => tuple,
      attrs => #{location => Location},
      content => to_map(Elements)};

%% Map

to_map({map, Attrs, Pairs}) ->
    #{type => map,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Pairs)};
to_map({map, Attrs, Var, Pairs}) ->
    #{type => map,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 var => to_map(Var)},
      content => to_map(Pairs)};

to_map({Type, Attrs, Key, Value}) when
      map_field_exact == Type;
      map_field_assoc == Type ->
    #{type => Type,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 key => to_map(Key),
                 value => to_map(Value)}};

%% List Comprehension

to_map({lc, Attrs, Expr, GeneratorsFilters}) ->
    LcExpr = to_map({lc_expr, Attrs, Expr}),
    LcGenerators = to_map(GeneratorsFilters),
    #{type => lc,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [LcExpr | LcGenerators]};

to_map({generate, Attrs, Pattern, Expr}) ->
    #{type => generate,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 pattern => to_map(Pattern),
                 expression => to_map(Expr)}};
to_map({lc_expr, Attrs, Expr}) ->
    #{type => lc_expr,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => [to_map(Expr)]};

%% Operation

to_map({op, Attrs, Operation, Left, Right}) ->
    #{type => op,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 operation => Operation},
      content => to_map([Left, Right])};

to_map({op, Attrs, Operation, Single}) ->
    #{type => op,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 operation => Operation},
      content => to_map([Single])};

%% Record

to_map({record, Attrs, Name, Fields}) ->
    #{type => record,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => Name},
      content => to_map(Fields)};
to_map({record, Attrs, Var, Name, Fields}) ->
    #{type => record,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 variable => to_map(Var),
                 name => Name},
      content => to_map(Fields)};

to_map({record_index, Attrs, Name, Field}) ->
    #{type => record_index,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => Name},
      content => [to_map(Field)]};

to_map({record_field, Attrs, Name}) ->
    #{type => record_field,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 name => to_map(Name)}};
to_map({record_field, Attrs, Name, Default}) ->
    #{type => record_field,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 default => to_map(Default),
                 name => to_map(Name)}};
to_map({record_field, Attrs, Var, Name, Field}) ->
    #{type => record_field,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 variable => to_map(Var),
                 name => Name},
      content => [to_map(Field)]};

%% Block

to_map({block, Attrs, Body}) ->
    #{type => block,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs)},
      content => to_map(Body)};

%% Attributes

to_map({attribute, Attrs, Type, Value}) ->
    #{type => Type,
      attrs => #{location => get_location(Attrs),
                 text => get_text(Attrs),
                 value => Value}};

%% Unhandled forms

to_map(Parsed) when is_tuple(Parsed) ->
    throw({unhandled_abstract_form, Parsed});
to_map(Parsed) ->
    throw({unexpected_abstract_form, Parsed}).
