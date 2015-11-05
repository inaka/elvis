-module(fail_no_debug_call).
-export([fail/0]).

-spec fail() -> any().
fail() ->
    io:format("debug print~n"),
    io:format("debug print ~s~n", ["debug info"]),
    % Sending explicit io to a device is not considered debugging
    io:format(user, "Not a debug print ~s~n", ["Not debug"]),
    ct:pal("Debug"),
    ct:pal("Debug ~s", ["Debug Info"]).
