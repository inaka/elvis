-module(fail_variable_naming_convention).

-export([bad_variables_name/2]).

%% CamelCase must be used for variables. Don’t
%% separate words in variables with _.

%% Cf. https://github.com/inaka/erlang_guidelines#variable-names

bad_variables_name(Im@Home, My_Way) ->
    Im@Home ++ My_Way.
