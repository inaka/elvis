-module(fail_function_naming_convention).

-export([camelCase/2,
         'ALL_CAPS'/2,
         has_number1/2,
         'ok-for-lisp'/2,
         'no_predicates?'/2,
         user@location/2
        ]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

camelCase(Should,Fail) ->
    [Should,Fail].

'ALL_CAPS'(Should,Fail) ->
    [Should,Fail].

has_number1(Should,Fail) ->
    [Should,Fail].

'ok-for-lisp'(Should,Fail) ->
    [Should,Fail].

'no_predicates?'(Should,Fail) ->
    [Should,Fail].

user@location(Should,Fail) ->
    [Should,Fail].

