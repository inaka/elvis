-module(fail_exec_path_length).

-compile([export_all]).

simple_function_ok() -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    io:format("5"),
    io:format("6"),
    io:format("7"),
    io:format("8"),
    io:format("9"),
    io:format("10").

simple_function_fail() -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    io:format("5"),
    io:format("6"),
    io:format("7"),
    io:format("8"),
    io:format("9"),
    io:format("10"),
    io:format("11").

function_case_first_branch_fail() -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    io:format("5"),
    io:format("6"),
    case hello of % 7
        1 ->
            io:format("8"),
            io:format("9"),
            io:format("10"),
            io:format("11");
        hello ->
            io:format("8"),
            io:format("9")
    end,
    io:format("12"),
    io:format("13"),
    io:format("14").


function_case_second_branch_fail() -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    io:format("5"),
    io:format("6"),
    case hello of % 7
        1 ->
            io:format("8"),
            io:format("9"),
            io:format("10");
        hello ->
            io:format("8"),
            io:format("9"),
            io:format("10"),
            io:format("11"),
            io:format("12")
    end.

function_case_ok() -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    io:format("5"),
    io:format("6"),
    case hello of % 7
        1 ->
            io:format("8"),
            io:format("9"),
            io:format("10");
        hello ->
            io:format("8"),
            io:format("9")
    end.

function_try_fail(Arg) ->
    io:format("2"),
    io:format("3"),
    try Arg of % 4
        1 ->
            io:format("5"),
            io:format("6");
        2 ->
            io:format("5"),
            io:format("6"),
            io:format("7"),
            io:format("8"),
            io:format("9"),
            io:format("10"),
            io:format("11");
        3 ->
            io:format("5"),
            io:format("6")
    catch
        _:_ ->
            ok
    end.


function_if_second_branch_fail(Arg) -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    if % 5
        Arg == 1 ->
            io:format("6"),
            io:format("7"),
            io:format("8"),
            io:format("9"),
            io:format("10");
        Arg == 2 ->
            io:format("6"),
            Result = 42 + Arg, % 7
            io:format("~p", [Result]), % 8
            io:format("9"),
            io:format("10"),
            io:format("11");
        true ->
            Var = 1,
            io:format(Var)
    end.

function_if_ok(Arg) -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    if % 5
        Arg == 1 ->
            io:format("6"),
            io:format("7"),
            io:format("8"),
            io:format("9"),
            io:format("10");
        Arg == 2 ->
            io:format("6"),
            io:format("7"),
            io:format("8"),
            io:format("9"),
            io:format("10");
        true ->
            Var = 1,
            io:format(Var)
    end.

function_if_case_fail(Arg) -> % 1
    io:format("2"),
    io:format("3"),
    io:format("4"),
    if % 5
        Arg == 1 ->
            io:format("6"),
            io:format("7"),
            io:format("8"),
            io:format("9"),
            io:format("10");
        Arg == 2 ->
            io:format("6"),
            io:format("7"),
            io:format("8"),
            case Arg of % 9
                1 ->
                    io:format("10");
                2 ->
                    io:format("10"),
                    io:format("11")
            end;
        true ->
            Var = 1,
            io:format(Var)
    end.
