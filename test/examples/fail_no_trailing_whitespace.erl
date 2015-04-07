-module(fail_no_trailing_whitespace).

-export([one/0, two/0, three/0, four/0, five/0]).

one() ->
    %% Following line ends with a tab
    not_ok.	

two() ->
    %% Following line ends with a space
    not_ok. 

three() -> 
    %% Previous line ends with a space
    not_ok.
 
four() -> %% Previous (supposedly blank) line has a space
    not_ok.

five() ->
    ok. %% This function should be fine
