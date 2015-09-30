-module(user_defined_rules).

-export([rule/3]).

rule(_Config, _Target, _) ->
    [elvis_result:new(item, "This will always FAIL.", [], 1)].
