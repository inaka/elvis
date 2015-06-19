-module(pass_dont_repeat_yourself).

-export([
         repeated_complexity_5/2,
         repeated_complexity_10/2
        ]).

repeated_complexity_5(X, Y) ->
    Z = X ++ [ok],
    W = Y ++ [error],
    Z ++ W ++ [something].

repeated_complexity_10(X, Y) ->
    Z = case X of
            Y -> io:format("Y");
            _ -> <<"ok">>
        end,

    case Z of
        X -> io:format("Y");
        _ -> <<"error">>
    end.
