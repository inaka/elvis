-module(fail_no_if_expression).

-export([
         uses_if/1,
         uses_if_twice/1
        ]).

uses_if(Arg) ->
    if
        Arg -> ok;
        Arg == false -> not_ok
    end,
    case 1 of
        1 -> ok;
        2 -> ok;
        3 -> ok
    end.

uses_if_twice(Arg) ->
    if
        Arg -> ok;
        Arg == false -> not_ok
    end,
    case 1 of
        1 -> ok;
        2 -> ok;
        3 -> ok
    end,
    if
        Arg -> ok;
        Arg == false -> not_ok
    end.
