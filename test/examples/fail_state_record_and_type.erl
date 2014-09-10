-module(fail_state_record_and_type).

-behavior(application).

-export([start/2, stop/1]).

-spec start(term(), term()) -> ok.
start(_, _) ->
    ok.

-spec stop(term()) -> ok.
stop(_) ->
    ok.
