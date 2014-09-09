-module(test_module).

-behavior(application).

-export([start/2, stop/1]).

-record(state, {}).

-type state() :: #state{}.
-export_type([state/0]).

-spec start(term(), term()) -> ok.
start(_, _) ->
    #state{}.

-spec stop(term()) -> ok.
stop(_) ->
    ok.
