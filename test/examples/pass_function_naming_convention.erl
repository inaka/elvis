-module(pass_function_naming_convention).

-export([snake_case/2]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

snake_case(Should, Pass) ->
    [Should, Pass].

