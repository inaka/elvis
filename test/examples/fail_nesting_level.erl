-module(fail_nesting_level).

-export([
         nested_four_levels/0
        ]).

nested_four_levels() ->
    case 1 of
        1 -> case 2 of
                 2 -> case 3 of
                          3 -> fourth
                      end
             end
    end,
    case 1 of
        1 -> case 2 of
                 2 -> case 3 of
                          3 -> fourth
                      end
             end
    end.
