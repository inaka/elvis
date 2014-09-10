-module(pass_state_record_and_type).

-behavior(application).

-export([start/2, stop/1]).

-record(state, {}).

-type state() :: #state{}.

-spec start(term(), state()) -> ok.
start(_, _) ->
    ok.

-spec stop(term()) -> ok.
stop(_) ->
    ok.
