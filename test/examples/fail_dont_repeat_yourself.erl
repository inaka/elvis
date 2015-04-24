-module(fail_dont_repeat_yourself).

repeated_complexity_5(X) ->
    X ++ [ok],
    Y ++ [ok],
    [ok].
