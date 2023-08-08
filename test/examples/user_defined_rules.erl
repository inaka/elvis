-module(user_defined_rules).

-export([rule/3]).

-hank([unnecessary_function_arguments]).

-spec rule(any(), any(), any()) -> [elvis_result:item(), ...].
rule(_Config, _Target, _) ->
    [elvis_result:new(item, "This will always FAIL.", [], 1)].
