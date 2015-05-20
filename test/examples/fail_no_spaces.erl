-module(fail_no_spaces).

-export([one/0, two/0, three/0, four/0]).

one() ->
  not_ok. %%This lines has a spaces

two() ->
		ok. %%This one doesn't
 
three() ->
	ok. %% This one doesn't

four() ->
    not_ok. %% This lines has 4 spaces
