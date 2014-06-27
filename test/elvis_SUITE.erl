-module(elvis_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         rock_with_empty_config/1,
         rock_with_incomplete_config/1,
         rock_with_file_config/1,
         check_configuration/1,
         find_file_and_check_src/1,
         verify_line_length_rule/1,
         verify_no_tabs_rule/1,
         main_help/1,
         main_commands/1,
         main_config/1,
         main_rock/1,
         main_default_config/1,
         main_unexistent/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = elvis_SUITE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    application:start(elvis),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(elvis),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rocking

-spec rock_with_empty_config(config()) -> any().
rock_with_empty_config(_Config) ->
    ok = try
             elvis:rock([]),
             fail
         catch
             throw:invalid_config -> ok
         end.

-spec rock_with_incomplete_config(config()) -> any().
rock_with_incomplete_config(_Config) ->
    ElvisConfig = [{src_dirs, ["src"]}],
    ok = try
             elvis:rock(ElvisConfig),
             fail
         catch
             throw:invalid_config -> ok
         end.

-spec rock_with_file_config(config()) -> ok.
rock_with_file_config(_Config) ->
    Fun = fun() -> elvis:rock() end,
    Expected = "# ../../test/examples/fail_line_length.erl [FAIL]\n",
    check_first_line_output(Fun, Expected),
    ok.

%%%%%%%%%%%%%%%
%%% Utils

-spec check_configuration(config()) -> any().
check_configuration(_Config) ->
    Config = [
              {src_dirs, ["src", "test"]},
              {rules, [{module, rule1, []}]}
             ],
    ["src", "test"] = elvis_utils:source_dirs(Config),
    [{module, rule1, []}] = elvis_utils:rules(Config).

-spec find_file_and_check_src(config()) -> any().
find_file_and_check_src(_Config) ->
    Dirs = ["../../test/examples"],

    [] = elvis_utils:find_files(Dirs, "doesnt_exist.erl"),
    [Path] = elvis_utils:find_files(Dirs, "small.erl"),

    {ok, <<"-module(small).\n">>} = elvis_utils:src([], Path),
    {error, enoent} = elvis_utils:src([], "doesnt_exist.erl").

%%%%%%%%%%%%%%%
%%% Rules

-spec verify_line_length_rule(config()) -> any().
verify_line_length_rule(_Config) ->
    ElvisConfig = application:get_all_env(elvis),
    SrcDirs = elvis_utils:source_dirs(ElvisConfig),

    File = "fail_line_length.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    Results = elvis_style:line_length(ElvisConfig, Path, [80]),
    ok = case length(Results) of
        2 -> ok;
        _ -> long_lines_undetected
    end.

-spec verify_no_tabs_rule(config()) -> any().
verify_no_tabs_rule(_Config) ->
    ElvisConfig = application:get_all_env(elvis),
    SrcDirs = elvis_utils:source_dirs(ElvisConfig),

    File = "fail_no_tabs.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    Results = elvis_style:no_tabs(ElvisConfig, Path, []),
    ok = case length(Results) of
        2 -> ok;
        _ -> tabs_undetected
    end.

%%%%%%%%%%%%%%%
%%% CLI

-spec main_help(config()) -> any().
main_help(_Config) ->
    Expected = "Usage: elvis",

    ShortOptFun = fun() -> elvis:main("-h") end,
    check_first_line_output(ShortOptFun, Expected, fun starts_with/2),

    LongOptFun = fun() -> elvis:main("--help") end,
    check_first_line_output(LongOptFun, Expected, fun starts_with/2),

    EmptyFun = fun() -> elvis:main("") end,
    check_first_line_output(EmptyFun, Expected, fun starts_with/2),

    CmdFun = fun() -> elvis:main("help") end,
    check_first_line_output(CmdFun, Expected, fun starts_with/2),

    ok.

-spec main_commands(config()) -> any().
main_commands(_Config) ->
    Expected = "Elvis will do the following things",

    OptFun = fun() -> elvis:main("--commands") end,
    check_first_line_output(OptFun, Expected, fun starts_with/2),

    ok.

-spec main_config(config()) -> any().
main_config(_Config) ->
    Expected = "Error: missing_option_arg config",

    OptFun = fun() -> elvis:main("-c") end,
    check_first_line_output(OptFun, Expected, fun starts_with/2),

    EnoentExpected = "Error: enoent.\n",
    OptEnoentFun = fun() -> elvis:main("-c missing") end,
    check_first_line_output(OptEnoentFun, EnoentExpected),

    ConfigFileFun = fun() -> elvis:main("-c ../../config/elvis.config") end,
    check_first_line_output(ConfigFileFun, ""),

    ok.

-spec main_rock(config()) -> any().
main_rock(_Config) ->
    ExpectedFail = "# ../../test/examples/fail_line_length.erl [FAIL]\n",

    NoConfigArgs = "rock",
    NoConfigFun = fun() -> elvis:main(NoConfigArgs) end,
    check_first_line_output(NoConfigFun, ExpectedFail, fun starts_with/2),

    Expected = "# ../../src/elvis.erl [OK]",

    ConfigArgs = "rock -c ../../config/elvis-test.config",
    ConfigFun = fun() -> elvis:main(ConfigArgs) end,
    check_first_line_output(ConfigFun, Expected, fun starts_with/2),

    ok.

-spec main_default_config(config()) -> any().
main_default_config(_Config) ->
    Src = "../../config/elvis-test.config",
    Dest = "./elvis.config",
    file:copy(Src, Dest),

    Expected = "# ../../src/elvis.erl [OK]",
    RockFun = fun() -> elvis:main("rock") end,
    check_first_line_output(RockFun, Expected, fun starts_with/2),

    file:delete(Dest),

    ok.

-spec main_unexistent(config()) -> any().
main_unexistent(_Config) ->
    Expected = "Error: unrecognized_or_unimplemened_command.\n",

    UnexistentFun = fun() -> elvis:main("aaarrrghh") end,
    check_first_line_output(UnexistentFun, Expected),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_first_line_output(Fun, Expected) ->
    Equals = fun(Result, Exp) ->
                 Result = Exp
             end,
    check_first_line_output(Fun, Expected, Equals).

check_first_line_output(Fun, Expected, CheckFun) ->
    ct:capture_start(),
    Fun(),
    ct:capture_stop(),
    Result = case ct:capture_get([]) of
                 [] -> "";
                 [Head | _] -> Head
             end,

    CheckFun(Result, Expected).

starts_with(Result, Expected) ->
    1 = string:str(Result, Expected).
