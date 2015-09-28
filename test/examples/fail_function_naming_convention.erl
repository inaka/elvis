-module(fail_function_naming_convention).

-export([bad_names_inside/0]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

bad_names_inside() ->
    camelCase(should, fail),
    'ALL_CAPS'(should, fail),
    'Initial_cap'(should, fail),
    'ok-for-lisp'(should, fail),
    'no_predicates?'(should, fail),
    user@location(should, fail).

%% Private / hidden functions still checked

camelCase(Should, Fail) ->
    [Should, Fail].

'ALL_CAPS'(Should, Fail) ->
    [Should, Fail].

'Initial_cap'(Should, Fail) ->
    [Should, Fail].

'ok-for-lisp'(Should, Fail) ->
    [Should, Fail].

'no_predicates?'(Should, Fail) ->
    [Should, Fail].

user@location(Should, Fail) ->
    [Should, Fail].

