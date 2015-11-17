-module(project_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         verify_no_deps_master_erlang_mk/1,
         verify_no_deps_master_rebar/1,
         verify_hex_dep_rebar/1,
         verify_git_for_deps_erlang_mk/1,
         verify_protocol_for_deps_erlang_mk/1,
         verify_git_for_deps_rebar/1,
         verify_protocol_for_deps_rebar/1,
         verify_old_config_format/1
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
    Exports = ?MODULE:module_info(exports),
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

-spec verify_no_deps_master_erlang_mk(config()) -> any().
verify_no_deps_master_erlang_mk(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "Makefile.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _] = elvis_project:no_deps_master_erlang_mk(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [sync]},
    [_, _] = elvis_project:no_deps_master_erlang_mk(ElvisConfig,
                                                    File,
                                                    RuleConfig),

    RuleConfig1 = #{ignore => [sync, meck]},
    [_] = elvis_project:no_deps_master_erlang_mk(ElvisConfig,
                                                 File,
                                                 RuleConfig1).

-spec verify_no_deps_master_rebar(config()) -> any().
verify_no_deps_master_rebar(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _] = elvis_project:no_deps_master_rebar(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [aleppo]},
    [_, _] = elvis_project:no_deps_master_rebar(ElvisConfig, File, RuleConfig),

    RuleConfig1 =  #{ignore => [aleppo, getopt]},
    [] = elvis_project:no_deps_master_rebar(ElvisConfig, File, RuleConfig1).


-spec verify_git_for_deps_erlang_mk(config()) -> any().
verify_git_for_deps_erlang_mk(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "Makefile.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _, _, _] =
        elvis_project:git_for_deps_erlang_mk(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [sync]},
    [_, _, _, _, _] = elvis_project:git_for_deps_erlang_mk(ElvisConfig,
                                                  File,
                                                  RuleConfig),

    RuleConfig1 = #{ignore => [sync, meck], regex => "git://.*"},
    [_] = elvis_project:git_for_deps_erlang_mk(ElvisConfig,
                                               File,
                                               RuleConfig1),

    RuleConfig2 = #{ignore => [sync], regex => "https://.*"},
    [_, _, _, _, _] = elvis_project:git_for_deps_erlang_mk(ElvisConfig,
                                                           File,
                                                           RuleConfig2).

-spec verify_protocol_for_deps_erlang_mk(config()) -> any().
verify_protocol_for_deps_erlang_mk(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "Makefile.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _, _, _] =
        elvis_project:protocol_for_deps_erlang_mk(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [sync]},
    [_, _, _, _, _] = elvis_project:protocol_for_deps_erlang_mk(ElvisConfig,
                                                  File,
                                                  RuleConfig),

    RuleConfig1 = #{ignore => [sync, meck], regex => "git://.*"},
    [_] = elvis_project:protocol_for_deps_erlang_mk(ElvisConfig,
                                               File,
                                               RuleConfig1),

    RuleConfig2 = #{ignore => [sync], regex => "https://.*"},
    [_, _, _, _, _] = elvis_project:protocol_for_deps_erlang_mk(ElvisConfig,
                                                           File,
                                                           RuleConfig2).

-spec verify_git_for_deps_rebar(config()) -> any().
verify_git_for_deps_rebar(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _] = elvis_project:git_for_deps_rebar(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [getopt]},
    [_, _] = elvis_project:git_for_deps_rebar(ElvisConfig, File, RuleConfig),

    RuleConfig1 =  #{ignore => [getopt, lager]},
    [] = elvis_project:git_for_deps_rebar(ElvisConfig, File, RuleConfig1),

    RuleConfig2 =  #{ignore => [meck], regex => "git@.*"},
    [_, _, _, _, _, _, _, _] =
        elvis_project:git_for_deps_rebar(ElvisConfig, File, RuleConfig2).

-spec verify_protocol_for_deps_rebar(config()) -> any().
verify_protocol_for_deps_rebar(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _] = elvis_project:protocol_for_deps_rebar(ElvisConfig, File, #{}),

    RuleConfig =  #{ignore => [getopt]},
    [_, _] = elvis_project:protocol_for_deps_rebar(ElvisConfig, File, RuleConfig),

    RuleConfig1 =  #{ignore => [getopt, lager]},
    [] = elvis_project:protocol_for_deps_rebar(ElvisConfig, File, RuleConfig1),

    RuleConfig2 =  #{ignore => [meck], regex => "git@.*"},
    [_, _, _, _, _, _, _, _] =
        elvis_project:protocol_for_deps_rebar(ElvisConfig, File, RuleConfig2).

-spec verify_hex_dep_rebar(config()) -> any().
verify_hex_dep_rebar(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar3.config.success",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [] = elvis_project:protocol_for_deps_rebar(ElvisConfig, File, #{}).

-spec verify_old_config_format(config()) -> any().
verify_old_config_format(_Config) ->
    ElvisConfig = elvis_config:default(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail.elvis.config",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail, #{}),

    PathFail1 = "fail.1.elvis.config",
    {ok, FileFail1} = elvis_test_utils:find_file(SrcDirs, PathFail1),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail1, #{}),

    PathFail2 = "fail.2.elvis.config",
    {ok, FileFail2} = elvis_test_utils:find_file(SrcDirs, PathFail2),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail2, #{}),

    PathPass = "pass.elvis.config",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_project:old_configuration_format(ElvisConfig, FilePass, #{}).
