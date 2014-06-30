-module(fail_line_length).

-export([
         function_1/0,
         function_2/0,
         function_3/0,
         function_4/0
        ]).

function_1() ->
    io:format("Hello").

function_2() ->
    io:format("This line is 81 characters long and should be detected, yeah!!!").

function_3() ->
    io:format("This line is 80 characters long and shouldn't be detected!!!!!").

function_4() ->
    io:format("This line is 90 characters long and should be detected!!!!!!!!!!!!!!!!!!").

%
