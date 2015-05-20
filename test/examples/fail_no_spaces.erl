-module(fail_no_spaces).

-export([one/0, two/0, three/0, four/0, five/0]).

one() ->
  not_ok. %%This lines has a spaces

two() ->
		ok. %%This one doesn't
             
three() ->
	ok. %% This one doesn't

four() ->
    not_ok. %% This lines has 4 spaces

five() ->
				x:this_line_is_good(),
				 x:this_line_is_wrong(),
								x:this_line(is, good),
								x:this_line(is, also,   good),
								x:this_line(should,  nott,      x:fail( either )),
        x:this_line_should(fail),
							  x:this_line_should(fail, as, well).