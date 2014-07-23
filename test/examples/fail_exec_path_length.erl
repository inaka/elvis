-module(fail_exec_path_length).

-export([
         long_function/0
        ]).

long_function() ->
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello"),
    io:format("Hello").
