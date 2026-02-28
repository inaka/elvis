-module(user_defined_rules).

-export([rule/2]).

-hank([unnecessary_function_arguments]).

-spec rule(any(), any()) -> [elvis_result:item(), ...].
rule(_Config, _Target) ->
    [elvis_result:new(item, "This will always FAIL.", [])].
