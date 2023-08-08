-module(elvis_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([run_webhook/1, run_webhook_ping/1, main_help/1, main_commands/1, main_config/1,
         main_version/1, main_rock/1, main_git_hook_fail/1, main_git_hook_ok/1,
         main_default_config/1, main_unexistent/1, main_code_path/1]).

                                                                               %% Webhook

         %% Mains & Commands

-define(EXCLUDED_FUNS, [module_info, all, test, init_per_suite, end_per_suite]).

-type config() :: proplists:proplist().

-export_type([config/0]).

-hank([unnecessary_function_arguments]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = elvis_SUITE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    _ = application:start(elvis),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Webhook

-spec run_webhook(config()) -> any().
run_webhook(_Config) ->
    Headers = #{<<"x-github-event">> => <<"pull_request">>},
    Path = "../../test/examples/pull_request.js",
    {ok, Body} = file:read_file(Path),
    Request = egithub_webhook_req:new(Headers, Body),

    try
        elvis:start(),

        meck:new(egithub, [passthrough]),
        Files = [#{<<"filename">> => <<"test/examples/rebar.config.fail">>}],
        FakeFun1 = fun(_, _, _) -> {ok, Files} end,
        meck:expect(egithub, pull_req_files, FakeFun1),

        EmptyResultFun = fun(_, _, _) -> {ok, []} end,
        meck:expect(egithub, pull_req_comments, EmptyResultFun),
        meck:expect(egithub, issue_comments, EmptyResultFun),
        meck:expect(egithub, pr_reviews, EmptyResultFun),

        FakeFun2 = fun(_, _, _, "elvis.config") -> {error, error} end,
        meck:expect(egithub, file_content, FakeFun2),

        ok = elvis_webhook:event(Request)
    after
        meck:unload(egithub)
    end.

-spec run_webhook_ping(config()) -> ok.
run_webhook_ping(_Config) ->
    Headers = #{<<"x-github-event">> => <<"ping">>},
    Body = <<"[]">>,
    Request = egithub_webhook_req:new(Headers, Body),
    ok = elvis_webhook:event(Request).

%%%%%%%%%%%%%%%
%%% Main & Commands

-spec main_help(config()) -> any().
main_help(_Config) ->
    Expected = "Usage: elvis",

    ShortOptFun = fun() -> elvis:main("-h") end,
    _ = check_first_line_output(ShortOptFun, Expected, fun starts_with/2),

    LongOptFun = fun() -> elvis:main("--help") end,
    _ = check_first_line_output(LongOptFun, Expected, fun starts_with/2),

    EmptyFun = fun() -> elvis:main("") end,
    _ = check_first_line_output(EmptyFun, Expected, fun starts_with/2),

    CmdFun = fun() -> elvis:main("help") end,
    _ = check_first_line_output(CmdFun, Expected, fun starts_with/2),

    ok.

-spec main_commands(config()) -> any().
main_commands(_Config) ->
    Expected = "Elvis will do the following things",

    OptFun = fun() -> elvis:main("--commands") end,
    _ = check_first_line_output(OptFun, Expected, fun starts_with/2),

    ok.

-spec main_config(config()) -> any().
main_config(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        Expected = "missing_option_arg config",

        OptFun = fun() -> elvis:main("-c") end,
        _ = check_first_line_output(OptFun, Expected, fun matches_regex/2),

        ConfigFun = fun() -> elvis:main("-c ../../config/elvis.config") end,
        check_empty_output(ConfigFun)
    after
        meck:unload(elvis_utils)
    end.

-spec main_version(config()) -> ok.
main_version(_Config) ->
    {ok, AppConfig} = application:get_all_key(elvis),
    Version = proplists:get_value(vsn, AppConfig),

    Expected = "Version: " ++ Version,
    OptFun = fun() -> elvis:main("--version") end,

    _ = check_some_line_output(OptFun, Expected, fun contains_string/2),
    ok.

-spec main_rock(config()) -> any().
main_rock(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        ExpectedFail = "# \\.\\./\\.\\./test/examples/.*\\.erl.*FAIL",

        NoConfigArgs = "rock",
        NoConfigFun = fun() -> elvis:main(NoConfigArgs) end,
        _ = check_some_line_output(NoConfigFun, ExpectedFail, fun matches_regex/2),

        Expected = "# ../../src/elvis.erl.*OK",

        ConfigArgs = "rock -V -c ../../config/elvis-test.config",
        ConfigFun = fun() -> elvis:main(ConfigArgs) end,
        _ = check_some_line_output(ConfigFun, Expected, fun matches_regex/2)
    after
        meck:unload(elvis_utils)
    end.

-spec main_git_hook_fail(config()) -> any().
main_git_hook_fail(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        meck:new(elvis_git, [passthrough]),
        meck:new(elvis_config, [passthrough]),
        LongLine =
            <<"Loooooooooooooooooooooooooooooooooooooooooooooooooooong ",
              "Liiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiine">>,
        Files =
            [#{path => "../../src/fake_long_line.erl", content => LongLine},
             #{path => "../../test/fake_long_line.erl", content => LongLine},
             #{path => "../../src/README.md", content => <<"### Title">>},
             #{path => "../../src/Makefile", content => <<"@Some text\n\nCT_OPTS =">>}],
        FakeStagedFiles = fun() -> Files end,
        meck:expect(elvis_git, staged_files, FakeStagedFiles),
        meck:expect(elvis_config, files, fun(_) -> Files end),

        Expected = "# ../../src/fake_long_line.erl.*FAIL",

        ConfigArgs = "git-hook -c ../../config/elvis-test.config",
        ConfigFun = fun() -> elvis:main(ConfigArgs) end,
        _ = check_some_line_output(ConfigFun, Expected, fun matches_regex/2),

        meck:expect(elvis_git, staged_files, fun() -> [] end),
        check_empty_output(ConfigFun)
    after
        meck:unload(elvis_utils),
        meck:unload(elvis_git)
    end.

-spec main_git_hook_ok(config()) -> any().
main_git_hook_ok(_Config) ->
    try
        meck:new(elvis_git, [passthrough]),
        meck:expect(elvis_git, staged_files, fun() -> [] end),

        ConfigArgs = "git-hook -c ../../config/elvis-test.config",
        ConfigFun = fun() -> elvis:main(ConfigArgs) end,
        check_empty_output(ConfigFun)
    after
        meck:unload(elvis_git)
    end.

-spec main_default_config(config()) -> any().
main_default_config(_Config) ->
    ok =
        try
            meck:new(elvis_utils, [passthrough]),
            meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),
            Src = "../../config/elvis-test.config",
            Dest = "./elvis.config",
            {ok, _} = file:copy(Src, Dest),

            Expected = "# ../../src/elvis.erl.*OK",
            RockFun = fun() -> elvis:main("rock") end,
            _ = check_some_line_output(RockFun, Expected, fun matches_regex/2),

            file:delete(Dest)
        after
            meck:unload(elvis_utils)
        end.

-spec main_unexistent(config()) -> any().
main_unexistent(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        Expected = "unrecognized_or_unimplemented_command.",

        UnexistentFun = fun() -> elvis:main("aaarrrghh") end,
        _ = check_first_line_output(UnexistentFun, Expected, fun matches_regex/2)
    after
        meck:unload(elvis_utils)
    end.

-spec main_code_path(config()) -> any().
main_code_path(_Config) ->
    Expected = "user_defined_rules.erl.*FAIL",
    Prefix = "../../",
    OutDir = Prefix ++ "ebin-test",
    Args = "rock -c " ++ Prefix ++ "config/elvis-test-pa.config --code-path " ++ OutDir,
    Source = Prefix ++ "test/examples/user_defined_rules.erl",
    Destination = Prefix ++ "ebin-test/user_defined_rules",

    _ = try
            meck:new(elvis_utils, [passthrough]),
            meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

            ok = file:make_dir(OutDir),
            {ok, _} = file:copy(Source, Destination ++ ".erl"),
            _ = compile:file(Destination, [{outdir, OutDir}]),

            CodePathFun = fun() -> elvis:main(Args) end,
            check_some_line_output(CodePathFun, Expected, fun matches_regex/2)
        after
            meck:unload(elvis_utils),
            ok = file:delete(Destination ++ ".erl"),
            ok = file:delete(Destination ++ ".beam"),
            file:del_dir(OutDir)
        end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_some_line_output(Fun, Expected, FilterFun) ->
    ct:capture_start(),
    Fun(),
    ct:capture_stop(),
    Lines = ct:capture_get([]),
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

check_first_line_output(Fun, Expected, FilterFun) ->
    ct:capture_start(),
    Fun(),
    ct:capture_stop(),
    Lines =
        case ct:capture_get([]) of
            [] ->
                [];
            [Head | _] ->
                [Head]
        end,
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

starts_with(Result, Expected) ->
    string:str(Result, Expected) == 1 orelse {Expected, Expected} == {Result, Expected}.

contains_string(Result, String) ->
    0 /= string:str(Result, String).

matches_regex(Result, Regex) ->
    nomatch =/= re:run(Result, Regex).

check_empty_output(Fun) ->
    Fun(),
    [] = ct:capture_get([]).
