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
    [to_map(Parsed)
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

-spec is_dot(tuple()) -> boolean().
is_dot({dot, _}) -> true;
is_dot(_) -> false.

%% @doc Converts a parse tree form the abstract format to a map based repr.
%% TODO: Attributes are not being handled correctly.
-spec to_map(term()) -> ok.
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
    #{type => var,
      attrs => #{location => Location,
                 function => to_map(Function),
                 arguments => to_map(Arguments)}};

to_map({remote, Location, Module, Function}) ->
    #{type => var,
      attrs => #{location => Location,
                 module => to_map(Module),
                 function => to_map(Function)}};

%% Keywords

to_map({'case', Location, Expression, Clauses}) ->
    #{type => 'case',
      attrs => #{location => Location,
                 expression => to_map(Expression)},
      content => to_map(Clauses)};

to_map({'fun', Location, {function, Name, Arity}}) ->
    #{type => 'fun',
      attrs => #{location => Location,
                 name => Name,
                 arity => Arity}};


to_map({'try', Location, Body, CaseClauses, CatchClauses, AfterBody}) ->
    #{type => 'try',
      attrs => #{location => Location,
                 case_clauses => to_map(CaseClauses),
                 catch_clauses => to_map(CatchClauses),
                 after_body => to_map(AfterBody)},
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
    #{type => lc,
      attrs => #{location => Location,
                 expression => to_map(Expr)},
      content => to_map(GeneratorsFilters)};
to_map({generate, Location, Pattern, Expr}) ->
    #{type => generate,
      attrs => #{location => Location,
                 pattern => to_map(Pattern),
                 expression => to_map(Expr)}};

%% Operation

to_map({op, Location, Operation, Left, Right}) ->
    #{type => op,
      attrs => #{location => Location,
                 operation => Operation},
      content => to_map([Left, Right])};

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
