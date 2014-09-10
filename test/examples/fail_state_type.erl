-module(fail_state_type).

-behavior(application).

-export([start/2, stop/1]).

-record(state, {}).

-spec start(term(), term()) -> ok.
start(_, _) ->
    #state{}.

-spec stop(term()) -> ok.
stop(_) ->
    ok.
