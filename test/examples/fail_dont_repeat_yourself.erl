-module(fail_dont_repeat_yourself).

-export([
         repeated_complexity_5/2,
         repeated_complexity_10/2
        ]).

repeated_complexity_5(X, Y) ->
    Z = X ++ [ok],
    W = Y ++ [ok],
    Z ++ W ++ [ok].

repeated_complexity_10(X, Y) ->
    case X of
        Y -> io:format("Y");
        _ -> ok
    end,

    case Y of
        X -> io:format("Y");
        _ -> ok
    end.
