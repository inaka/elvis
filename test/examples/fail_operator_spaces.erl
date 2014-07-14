-module(fail_operator_spaces).

-export([function1/2,function2/2]).

function1(Should,Fail) ->
  [Should,Fail].

function2(Shouldnt, Fail) ->
  _Unless = [we, consider]++ [operands, as, well],
  _WithDash = Shouldnt - Fail.
