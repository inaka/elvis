-module(elvis_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         %% Rocking
         rock_with_empty_map_config/1,
         rock_with_empty_list_config/1,
         rock_with_incomplete_config/1,
         rock_with_list_config/1,
         rock_with_file_config/1,
         rock_with_old_config/1,
         rock_this/1,
         rock_without_colors/1,
         %% Webhook
         run_webhook/1,
         run_webhook_ping/1,
         %% Utill & Config
         throw_configuration/1,
         find_file_and_check_src/1,
         invalid_file/1,
         to_string/1,
         %% Mains & Commands
         main_help/1,
         main_commands/1,
         main_config/1,
         main_version/1,
         main_rock/1,
         main_git_hook_fail/1,
         main_git_hook_ok/1,
         main_default_config/1,
         main_unexistent/1,
         main_code_path/1
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

-spec rock_with_empty_map_config(config()) -> any().
rock_with_empty_map_config(_Config) ->
    ok = try
             elvis:rock(#{}),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end,
    ok = try
             elvis:rock([]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_empty_list_config(config()) -> any().
rock_with_empty_list_config(_Config) ->
    ok = try
             elvis:rock([#{}, #{}]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_incomplete_config(config()) -> any().
rock_with_incomplete_config(_Config) ->
    ElvisConfig = #{src_dirs => ["src"]},
    ok = try
             elvis:rock(ElvisConfig),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_list_config(config()) -> any().
rock_with_list_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"],
                     rules => []},
                   #{dirs => ["."],
                     filter => "Makefile",
                     rules => []}],
    ok = try
             elvis:rock(ElvisConfig),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_with_file_config(config()) -> ok.
rock_with_file_config(_Config) ->
    Fun = fun() -> elvis:rock() end,
    Expected = "# \\.\\./\\.\\./test/examples/.*\\.erl.*FAIL",
    check_some_line_output(Fun, Expected, fun matches_regex/2),
    ok.

-spec rock_with_old_config(config()) -> ok.
rock_with_old_config(_Config) ->
    ConfigPath = "../../config/old/elvis.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    ok = try
             elvis:rock(ElvisConfig),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath1 = "../../config/old/elvis-test.config",
    ElvisConfig1 = elvis_config:load_file(ConfigPath1),
    ok = try
             elvis:rock(ElvisConfig1),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath2 = "../../config/old/elvis-test-rule-config-list.config",
    ElvisConfig2 = elvis_config:load_file(ConfigPath2),
    ok = try
             elvis:rock(ElvisConfig2),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_this(config()) -> ok.
rock_this(_Config) ->
    ok = elvis:rock_this(elvis),

    ok = try
             elvis:rock_this("bla.erl")
         catch
             _:{enoent, "bla.erl"} -> ok
         end,

    Path = "../../test/examples/fail_god_modules.erl",
    {fail, _} = elvis:rock_this(Path),

    ok.

-spec rock_without_colors(config()) -> ok.
rock_without_colors(_Config) ->
    ConfigPath = "../../config/test.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    Fun = fun() -> elvis:rock(ElvisConfig) end,
    Expected = "\\e.*?m",
    ok = try check_some_line_output(Fun, Expected, fun matches_regex/2) of
             Result -> ct:fail("Unexpected result ~p", [Result])
         catch
             _:{badmatch, []} -> ok
         end.

%%%%%%%%%%%%%%%
%%% Webhook

-spec run_webhook(config()) -> any().
run_webhook(_Config) ->
    Headers = #{<<"x-github-event">> => <<"pull_request">>},
    Path = "../../test/examples/pull_request.js",
    {ok, Body} = file:read_file(Path),
    Request = #{headers => Headers, body => Body},

    try
        elvis:start(),

        meck:new(egithub, [passthrough]),
        Files = [#{<<"filename">> => <<"test/examples/rebar.config.fail">>}],
        FakeFun1 = fun(_, _, _) -> {ok, Files} end,
        meck:expect(egithub, pull_req_files, FakeFun1),

        EmptyResultFun = fun(_, _, _) -> {ok, []} end,
        meck:expect(egithub, pull_req_comments, EmptyResultFun),
        meck:expect(egithub, issue_comments, EmptyResultFun),

        FakeFun2 = fun(_, _, _, "elvis.config") ->
                           {error, error}
                   end,
        meck:expect(egithub, file_content, FakeFun2),

        ok = elvis:webhook(Request)
    after
        meck:unload(egithub)
    end.

-spec run_webhook_ping(config()) -> any().
run_webhook_ping(_Config) ->
    Headers = #{<<"x-github-event">> => <<"ping">>},
    Body = <<"[]">>,
    Request = #{headers => Headers, body => Body},
    ok = elvis:webhook(Request).

%%%%%%%%%%%%%%%
%%% Utils

-spec throw_configuration(config()) -> any().
throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    ok = try
             elvis_config:default(),
             fail
         catch
             throw:_ -> ok
         after
             file:delete(Filename)
         end.

-spec find_file_and_check_src(config()) -> any().
find_file_and_check_src(_Config) ->
    Dirs = ["../../test/examples"],

    [] = elvis_file:find_files(Dirs, "doesnt_exist.erl"),
    [File] = elvis_file:find_files(Dirs, "small.erl"),

    {<<"-module(small).\n">>, _} = elvis_file:src(File),
    {error, enoent} = elvis_file:src(#{path => "doesnt_exist.erl"}).

-spec invalid_file(config()) -> any().
invalid_file(_Config) ->
    ok = try
             elvis_file:src(#{}),
             fail
         catch
             throw:{invalid_file, #{}} -> ok
         end.

-spec to_string(config()) -> any().
to_string(_Config) ->
    "1" = elvis_utils:to_str(1),
    "hello" = elvis_utils:to_str(<<"hello">>),
    "atom" = elvis_utils:to_str(atom).

%%%%%%%%%%%%%%%
%%% Main & Commands

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
    Expected = "missing_option_arg config",

    OptFun = fun() -> elvis:main("-c") end,
    check_first_line_output(OptFun, Expected, fun matches_regex/2),

    EnoentExpected = "enoent",
    OptEnoentFun = fun() -> elvis:main("-c missing") end,
    check_first_line_output(OptEnoentFun, EnoentExpected, fun matches_regex/2),

    ConfigFun = fun() -> elvis:main("-c ../../config/elvis.config") end,
    check_empty_output(ConfigFun),
    ok.

-spec main_version(config()) -> ok.
main_version(_Config) ->
    {ok, AppConfig} = application:get_all_key(elvis),
    Version = proplists:get_value(vsn, AppConfig),

    Expected = ".*Version: " ++ Version,
    OptFun = fun() -> elvis:main("--version") end,

    check_some_line_output(OptFun, Expected, fun matches_regex/2),
    ok.

-spec main_rock(config()) -> any().
main_rock(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        ExpectedFail = "# \\.\\./\\.\\./test/examples/.*\\.erl.*FAIL",

        NoConfigArgs = "rock",
        NoConfigFun = fun() -> elvis:main(NoConfigArgs) end,
        check_some_line_output(NoConfigFun, ExpectedFail, fun matches_regex/2),

        Expected = "# ../../src/elvis.erl.*OK",

        ConfigArgs = "rock -c ../../config/elvis-test.config",
        ConfigFun = fun() -> elvis:main(ConfigArgs) end,
        check_some_line_output(ConfigFun, Expected, fun matches_regex/2),

        ok
    after
        meck:unload(elvis_utils)
    end.

-spec main_git_hook_fail(config()) -> any().
main_git_hook_fail(_Config) ->
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        meck:new(elvis_git, [passthrough]),
        LongLine = <<"Loooooooooooooooooooooooooooooooooooooooooooooooooooong ",
                     "Liiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiine">>,
        Files = [
                 #{path => "../../src/fake_long_line.erl",
                   content => LongLine},
                 #{path => "../../test/fake_long_line.erl",
                   content => LongLine},
                 #{path => "../../src/README.md",
                   content => <<"### Title">>},
                 #{path => "../../src/Makefile",
                   content => <<"@Some text\n\nCT_OPTS =">>}
                ],
        FakeStagedFiles = fun() -> Files end,
        meck:expect(elvis_git, staged_files, FakeStagedFiles),

        Expected = "# ../../src/fake_long_line.erl.*FAIL",

        ConfigArgs = "git-hook -c ../../config/elvis-test.config",
        ConfigFun = fun() -> elvis:main(ConfigArgs) end,
        check_some_line_output(ConfigFun, Expected, fun matches_regex/2),

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
    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),
        Src = "../../config/elvis-test.config",
        Dest = "./elvis.config",
        file:copy(Src, Dest),

        Expected = "# ../../src/elvis.erl.*OK",
        RockFun = fun() -> elvis:main("rock") end,
        check_some_line_output(RockFun, Expected, fun matches_regex/2),

        file:delete(Dest),
        ok
    after
        meck:unload(elvis_utils)
    end.

-spec main_unexistent(config()) -> any().
main_unexistent(_Config) ->
    Expected = "unrecognized_or_unimplemented_command.",

    UnexistentFun = fun() -> elvis:main("aaarrrghh") end,
    check_first_line_output(UnexistentFun, Expected, fun matches_regex/2),

    ok.

-spec main_code_path(config()) -> any().
main_code_path(_Config) ->
    Expected = "user_defined_rules.erl.*FAIL",
    Prefix = "../../",
    OutDir = Prefix ++ "ebin-test",
    Args = "rock -c "
        ++ Prefix
        ++ "config/elvis-test-pa.config --code-path "
        ++ OutDir,
    Source = Prefix ++ "test/examples/user_defined_rules.erl",
    Destination = Prefix ++ "ebin-test/user_defined_rules",

    try
        meck:new(elvis_utils, [passthrough]),
        meck:expect(elvis_utils, erlang_halt, fun(Code) -> Code end),

        file:make_dir(OutDir),
        file:copy(Source, Destination ++ ".erl"),
        compile:file(Destination, [{outdir, OutDir}]),

        CodePathFun = fun() -> elvis:main(Args) end,
        check_some_line_output(CodePathFun, Expected, fun matches_regex/2)
    after
        meck:unload(elvis_utils),
        file:delete(Destination ++ ".erl"),
        file:delete(Destination ++ ".beam"),
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
    Lines = case ct:capture_get([]) of
                [] -> [];
                [Head | _] -> [Head]
            end,
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

starts_with(Result, Expected) ->
    case string:str(Result, Expected) of
        1 -> true;
        _ -> {Expected, Expected} == {Result, Expected}
    end.

matches_regex(Result, Regex) ->
    case re:run(Result, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.

check_empty_output(Fun) ->
    Fun(),
    [] = ct:capture_get([]).
