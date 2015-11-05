-module(pass_variable_naming_convention).

-export([should_pass/5]).

%% CamelCase must be used for variables. Don’t
%% separate words in variables with _.

%% Cf. https://github.com/inaka/erlang_guidelines#variable-names

should_pass(Should, Pass, Way2Home, Fun1, Fun2) ->
    {_IgnoredVariable, _} = ["Ignored but valid", "also valid"],
    Should = "Should",
    Pass = "Pass",
    Way2Home = "Way to home",
    Fun1 = Should ++ Pass,
    Fun2 = Fun1 ++ Way2Home.
