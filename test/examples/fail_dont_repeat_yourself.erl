-module(fail_dont_repeat_yourself).

-export([
         repeated_complexity_5/2,
         repeated_complexity_10/2
        ]).

repeated_complexity_5(X, Y) ->
    Z = X ++ [ok],
    W = Y ++ [ok],
    Z ++ W ++ [ok].

-spec repeated_complexity_10(any(), any()) -> fun((...) -> any()).
repeated_complexity_10(X, Y) ->
    Z = case X of
            Y -> io:format("Y");
            _ -> <<"ok">>
        end,

    W = case Z of
            X -> io:format("Y");
            _ -> <<"ok">>
        end,

    fun(_) -> W end.
