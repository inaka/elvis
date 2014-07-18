-module(fail_nesting_level).

%% Used so that the line positions don't change for tests.
-compile([export_all]).

exceed_with_four_levels() ->
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

exceed_at_diff_branches() ->
    case 1 of
        1 -> ok;
        2 -> case 2 of
                 1 -> ok;
                 2 -> ok;
                 3 -> if
                          true -> true;
                          false -> false
                      end;
                 4 -> ok
             end;
        3 -> 3
    end.

exceed_with_try_of() ->
    case 1 of
        1 -> ok;
        2 -> try 2 of
                 1 -> ok;
                 2 -> ok;
                 3 -> if
                          true -> true;
                          false -> false
                      end;
                 4 -> ok
             catch
                 _:_ -> ok
             end;
        3 -> 3
    end.

dont_exceed_with_try() ->
    case 1 of
        1 -> ok;
        2 -> try
                 2,
                 if
                     true -> true;
                     false -> false
                 end
             catch
                 _:_ -> ok
             end;
        3 -> 3
    end.

exceed_with_try_catch() ->
    case 1 of
        1 -> ok;
        2 -> try
                 2
             catch
                 _:_ ->
                     if
                         true -> true;
                         false -> false
                     end
             end;
        3 -> 3
    end.

dont_exceed_with_try_after() ->
    case 1 of
        1 -> ok;
        2 -> try
                 2
             catch
                 _:_ -> ok
             after
                 if
                     true -> true;
                     false -> false
                 end
             end;
        3 -> 3
    end.

dont_exceed_with_catch() ->
    case 1 of
        1 -> ok;
        2 -> catch
                 if
                     true -> true;
                     false -> false
                 end;
        3 -> 3
    end.

exceed_with_receive() ->
    case 1 of
        1 -> ok;
        2 -> receive
                 1 -> ok;
                 2 -> ok;
                 3 ->
                     if
                         bla -> true;
                         false -> false
                     end;
                 4 -> ok
             end;
        3 -> 3
    end.

dont_exceed_with_receive_after() ->
    case 1 of
        1 -> ok;
        2 -> receive
                 1 -> ok;
                 2 -> ok;
                 3 -> ok
             after
                 1000 ->
                     if
                         true -> true;
                         false -> false
                     end
             end;
        3 -> 3
    end.

dont_exceed_with_list_compr() ->
    case 1 of
        1 -> ok;
        2 -> receive
                 1 -> ok;
                 2 -> ok;
                 3 -> ok
             after
                 1000 ->
                     [X || X <- [1, 2, 3]]
             end;
        3 -> 3
    end.

exceed_with_list_compr() ->
    case 1 of
        1 -> ok;
        2 -> receive
                 1 -> ok;
                 2 -> ok;
                 3 -> [case X of
                           1 -> ok;
                           _ -> not_ok
                       end
                       || X <- [1, 2, 3]];
                 4 -> ok
             end;
        3 -> 3
    end.
