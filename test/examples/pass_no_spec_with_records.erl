-module(pass_no_spec_with_records).

-export([
         function_1/1,
         function_2/2,
         function_3/2
        ]).

-record(state, {}).

-spec function_1(atom()) -> atom().
function_1(Arg) ->
    Arg.

-spec function_2(atom(), atom()) -> #{}.
function_2(_Arg1, _Arg2) ->
    #state{}.

-spec function_3(atom(), ok | error) -> atom().
function_3(_Arg1, _Arg2) ->
    ok.
