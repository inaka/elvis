-module(elvis_code).

%% General
-export([
         find/2,
         find_zipper/2,
         find_by_location/2
        ]).

%% Specific
-export([
         past_nesting_limit/2,
         exported_functions/1,
         module_name/1,
         print_node/1,
         print_node/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Finds all nodes that comply with the predicate function.
-spec find(fun((ktn_code:tree_node()) ->
    boolean()), ktn_code:tree_node()) -> [ktn_code:tree_node()].
find(Pred, Node) ->
    Results = find(Pred, Node, []),
    lists:flatten(Results).

find(_Pred, [], Results) ->
    Results;
find(Pred, [Node | Nodes], Results) ->
    [find(Pred, Node, Results) | find(Pred, Nodes, [])];
find(Pred, Node, Results) ->
    Content = ktn_code:content(Node),
    case Pred(Node) of
        true ->
            find(Pred, Content, [Node | Results]);
        false ->
            find(Pred, Content, Results)
    end.

-spec find_zipper(fun((zipper:zipper()) -> boolean()), ktn_code:tree_node()) ->
    [ktn_code:tree_node()].
find_zipper(Pred, Root) ->
    IsBranch = fun
                   (#{content := [_ | _]}) -> true;
                   (_) -> false
               end,
    Children = fun (#{content := Content}) -> Content end,
    MakeNode = fun(Node, _) -> Node end,
    Zipper = zipper:new(IsBranch, Children, MakeNode, Root),
    Results = find_zipper(Pred, Zipper, []),
    lists:reverse(Results).

find_zipper(Pred, Zipper, Results) ->
    case zipper:is_end(Zipper) of
        true ->
            Results;
        false ->
            Node = zipper:node(Zipper),
            NewResults = case Pred(Zipper) of
                             true -> [Node | Results];
                             false -> Results
                         end,
            find_zipper(Pred, zipper:next(Zipper), NewResults)
    end.


-spec find_by_location(ktn_code:tree_node(), {integer(), integer()}) ->
    not_found | {ok, ktn_code:tree_node()}.
find_by_location(Root, {Line, Column}) ->
    Fun = fun
              (Node = #{attrs := #{location := {NodeLine, NodeCol}}})
                when NodeLine == Line->
                  Text = ktn_code:attr(text, Node),
                  Length = length(Text),
                  (NodeCol =< Column) and (Column =< NodeCol + Length);
              (_) ->
                  false
          end,
    case find(Fun, Root) of
        [] -> not_found;
        [Node | _] ->
            {ok, Node}
    end.

%%% Processing functions

%% @doc Takes a node and returns all nodes where the nesting limit is exceeded.
-spec past_nesting_limit(ktn_code:tree_node(), integer()) ->
    [{ktn_code:tree_node(), integer()}].
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
-spec print_node(ktn_code:tree_node()) -> ok.
print_node(Node) ->
    print_node(Node, 0).

-spec print_node(ktn_code:tree_node(), integer()) -> ok.
print_node(Node = #{type := Type}, CurrentLevel) ->
    Type = ktn_code:type(Node),
    Indentation = lists:duplicate(CurrentLevel * 4, $ ),
    Content = ktn_code:content(Node),

    lager:info(
      "~s - [~p] ~p~n",
      [Indentation, CurrentLevel, Type]
     ),
    lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.

%% @doc Takes the root node and returns the module's name.
-spec module_name(ktn_code:tree_node()) -> atom().
module_name(#{type := root, content := Content}) ->
    Fun = fun (#{type := Type}) -> Type == module end,
    case lists:filter(Fun, Content) of
        [ModuleNode | _] ->
            ktn_code:attr(value, ModuleNode);
        [] -> undefined
    end.

%% @doc Takes the root node of a parse_tree and returns name and artity
%%      of each exported functions.
-spec exported_functions(ktn_code:tree_node()) -> [{atom(), integer()}].
exported_functions(#{type := root, content := Content}) ->
    Fun = fun
              (Node = #{type := export}) ->
                  ktn_code:attr(value, Node);
              (_) -> []
          end,
    lists:flatmap(Fun, Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Takes a node type and determines its nesting level increment.
level_increment(Type) ->
    IncrementOne = [function,
                    'case',
                    'if',
                    try_case,
                    try_catch,
                    'fun',
                    named_fun,
                    receive_case
                   ],
    case lists:member(Type, IncrementOne) of
        true -> 1;
        false -> 0
    end.
