-module(fail_no_tabs).

-export([one/0, two/0, three/0, four/0]).

one() ->
	not_ok. %%This lines has a tab

two() ->
    ok. %%This one doesn't
 
three() ->
 ok. %% This one doesn't

four() ->
		not_ok. %% This lines has 2 tabs
