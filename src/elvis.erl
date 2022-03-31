-module(elvis).

%% Public API

-export([main/1, default_config/0]).

-elvis([{elvis_style, no_debug_call, disable}]).

-export([start/0]).

-define(APP_NAME, "elvis").
-define(DEFAULT_CONFIG_PATH, "./elvis.config").
-define(DEFAULT_REBAR_CONFIG_PATH, "./rebar.config").

-type option() ::
    commands |
    help |
    keep_rocking |
    quiet |
    verbose |
    version |
    {code_path, [any()]} |
    {config, [any()]} |
    {output_format, [any()]} |
    {parallel, [any()]}.

-export_type([option/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis),
    ok.

-spec main(string()) -> ok.
main(Args) ->
    %% Load the application to be able to access its information
    %% (e.g. --version option)
    ok =
        case application:load(elvis) of
            ok ->
                ok;
            {error, {already_loaded, elvis}} ->
                ok
        end,
    ok =
        case application:load(elvis_core) of
            ok ->
                ok;
            {error, {already_loaded, elvis_core}} ->
                ok
        end,
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[], []}} ->
            help();
        {ok, {Options, Commands}} ->
            process_options(Options, Commands);
        {error, {Reason, Data}} ->
            elvis_utils:error_prn("~s ~p~n", [Reason, Data]),
            help(),
            elvis_utils:erlang_halt(1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Command Line Interface

%% @private
-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Commands =
        "Provide the path to the configuration file. "
        "When none is provided elvis checks if there's "
        "an elvis.config file.",
    OutputFormat =
        "It allows you to display the results in plain text. When "
        "none is provided elvis displays the results in colors. "
        "The options allowed are (plain | colors | parsable).",
    KeepRocking =
        "Won't stop rocking on first error"
        " when given a list of files",
    Parallel =
        "Allows to analyze files concurrently. Provide max number of"
        " concurrent workers, or specify \"auto\" to peek default value"
        " based on the number of schedulers.",
    [{help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Commands},
     {commands, undefined, "commands", undefined, "Show available commands."},
     {output_format, undefined, "output-format", string, OutputFormat},
     {parallel, $P, "parallel", string, Parallel},
     {quiet, $q, "quiet", undefined, "Suppress all output."},
     {verbose, $V, "verbose", undefined, "Enable verbose output."},
     {version, $v, "version", undefined, "Output the current elvis version."},
     {code_path, $p, "code-path", string, "Add the directory in the code path."},
     {keep_rocking, $k, "keep-rocking", undefined, KeepRocking}].

%% @private
-spec process_options([option()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = default_config(),
        AtomCommands = lists:map(fun list_to_atom/1, Commands),
        process_options(Options, AtomCommands, Config)
    catch
        _:Exception ->
            elvis_utils:error_prn("~p.", [Exception]),
            elvis_utils:erlang_halt(1)
    end.

%% @private
-spec process_options([option()], [string()], elvis_config:configs()) -> ok.
process_options([help | Opts], Cmds, Config) ->
    help(),
    process_options(Opts, Cmds, Config);
process_options([{config, Path} | Opts], Cmds, _) ->
    Config = elvis_config:from_file(Path),
    process_options(Opts, Cmds, Config);
process_options([commands | Opts], Cmds, Config) ->
    commands(),
    process_options(Opts, Cmds, Config);
process_options([{output_format, Format} | Opts], Cmds, Config) ->
    ok = application:set_env(elvis_core, output_format, list_to_atom(Format)),
    process_options(Opts, Cmds, Config);
process_options([keep_rocking | Opts], Cmds, Config) ->
    ok = application:set_env(elvis_core, keep_rocking, true),
    process_options(Opts, Cmds, Config);
process_options([quiet | Opts], Cmds, Config) ->
    ok = application:set_env(elvis_core, no_output, true),
    process_options(Opts, Cmds, Config);
process_options([verbose | Opts], Cmds, Config) ->
    ok = application:set_env(elvis_core, verbose, true),
    process_options(Opts, Cmds, Config);
process_options([version | Opts], Cmds, Config) ->
    version(),
    process_options(Opts, Cmds, Config);
process_options([{code_path, Path} | Opts], Cmds, Config) ->
    true = code:add_path(Path),
    process_options(Opts, Cmds, Config);
process_options([{parallel, Num} | Opts], Cmds, Config) ->
    N = case Num of
            "auto" ->
                erlang:system_info(schedulers);
            _ ->
                erlang:list_to_integer(Num)
        end,
    ok = application:set_env(elvis_core, parallel, N),
    process_options(Opts, Cmds, Config);
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config).

%% @private
-spec process_commands([rock |
                        help |
                        [install | 'git-hook'] |
                        'git-hook' |
                        'git-branch' |
                        string()],
                       elvis_config:configs()) ->
                          ok.
process_commands([rock], Config) ->
    case elvis_core:rock(Config) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end;
process_commands([rock | Files], Config) ->
    Paths = [file_to_path(File) || File <- Files],
    NewConfig = elvis_config:resolve_files(Config, Paths),
    case elvis_core:rock(NewConfig) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end;
process_commands([help | Cmds], Config) ->
    Config = help(Config),
    process_commands(Cmds, Config);
process_commands([install, 'git-hook' | Cmds], Config) ->
    elvis_git:install_hook(),
    process_commands(Cmds, Config);
process_commands(['git-hook' | Cmds], Config) ->
    elvis_git:run_hook(Config),
    process_commands(Cmds, Config);
process_commands(['git-branch', Commit | Cmds], Config) ->
    elvis_git:run_branch(atom_to_list(Commit), Config),
    process_commands(Cmds, Config);
process_commands([], _Config) ->
    ok;
process_commands([_Cmd | _Cmds], _Config) ->
    error(unrecognized_or_unimplemented_command).


file_to_path(File) ->
    Path = atom_to_list(File),
    Dirname = filename:dirname(Path),
    Filename = filename:basename(Path),
    case elvis_file:find_files([Dirname], Filename) of
        [] -> throw({enoent, Path});
        [File0] -> File0
    end.

%%% Options

%% @private
-spec help() -> ok.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, ?APP_NAME, standard_io).

%% @private
-spec help(elvis_config:configs()) -> elvis_config:configs().
help(Config) ->
    help(),
    Config.

%% @private
-spec commands() -> ok.
commands() ->
    Commands =
        <<"Elvis will do the following things for you when asked nicely:

rock [file...]   Rock your socks off by running all rules to your source files.

git-hook         Pre-commit Git Hook: Gets all staged files and runs the rules
                                      specified in the configuration to those
                                      files.

git-branch [branch|commit]
                 Rock your socks off by running all rules on source files that
                 have changed since branch or commit.

install git-hook
                Installs Elvis as a pre-commit hook in your current working
                directory, which should be a git repository.
">>,
    io:put_chars(Commands).

%% @private
-spec version() -> ok.
version() ->
    {ok, ElvisCoreAppConfig} = application:get_all_key(elvis_core),
    {ok, ElvisShellAppConfig} = application:get_all_key(elvis),
    ElvisCoreVsn = proplists:get_value(vsn, ElvisCoreAppConfig),
    ElvisShellVsn = proplists:get_value(vsn, ElvisShellAppConfig),
    Version =
        "   ______     _   \n"
        "  / __/ /  __(_)__\n"
        " / _// / |/ / (_-<\n"
        "/___/_/|___/_/___/\n"
        "Version: ~s\n"
        "Elvis Core Version: ~s\n",
    io:format(Version, [ElvisShellVsn, ElvisCoreVsn]).

-spec default_config() -> elvis_config:configs().
default_config() ->
    default_config([fun() -> elvis_config:from_file(?DEFAULT_CONFIG_PATH) end,
                    fun() -> elvis_config:from_rebar(?DEFAULT_REBAR_CONFIG_PATH) end]).

-spec default_config([Fun]) -> elvis_config:configs()
    when Fun :: fun(() -> elvis_config:config()).
default_config([Fun | Funs]) ->
    Config =
        try
            Fun()
        catch
            _:_ ->
                []
        end,
    case Config of
        [] ->
            default_config(Funs);
        Config ->
            Config
    end;
default_config([]) ->
    application:get_env(elvis, config, []).
