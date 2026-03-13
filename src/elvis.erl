-module(elvis).

%% Public API

-export([main/1]).

-elvis([{elvis_style, no_debug_call, disable}]).

-export([start/0]).

-type option() ::
    commands
    | help
    | quiet
    | verbose
    | version
    | {code_path, [string()]}
    | {config, string()}
    | {output_format, string()}
    | {parallel, string()}
    | {warnings_as_errors, string()}.

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
    _ = application:load(elvis),
    _ = application:load(elvis_core),
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[], []}} ->
            help();
        {ok, {Options, Commands}} ->
            process_options(Options, Commands);
        {error, {Reason, Data}} ->
            elvis_utils:error("~s ~p~n", [Reason, Data]),
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
    Parallel =
        "Allows to analyze files concurrently. Provide max number of"
        " concurrent workers, or specify \"auto\" to peek default value"
        " based on the number of schedulers.",
    WarningsAsErrors =
        "When set to false (default=true), the process will return a success"
        " exit code (0) even if warnings or errors are detected.",
    [
        {help, $h, "help", undefined, "Show this help information."},
        {config, $c, "config", string, Commands},
        {commands, undefined, "commands", undefined, "Show available commands."},
        {output_format, undefined, "output-format", string, OutputFormat},
        {parallel, $P, "parallel", string, Parallel},
        {warnings_as_errors, $e, "warnings_as_errors", boolean, WarningsAsErrors},
        {quiet, $q, "quiet", undefined, "Suppress all output."},
        {verbose, $V, "verbose", undefined, "Enable verbose output."},
        {version, $v, "version", undefined, "Output the current elvis version."},
        {code_path, $p, "code-path", string, "Add the directory in the code path."}
    ].

%% @private
-spec process_options([option()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = default_config(),
        AtomCommands = lists:map(fun list_to_atom/1, Commands),
        process_options(Options, AtomCommands, Config)
    catch
        _:Exception ->
            elvis_utils:error("~p.", [Exception]),
            elvis_utils:erlang_halt(1)
    end.

%% @private
-spec process_options([option()], [string()], [elvis_config:t()]) -> ok.
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
    ok = elvis_config:set_output_format(list_to_existing_atom(Format)),
    process_options(Opts, Cmds, Config);
process_options([quiet | Opts], Cmds, Config) ->
    ok = elvis_config:set_no_output(true),
    process_options(Opts, Cmds, Config);
process_options([verbose | Opts], Cmds, Config) ->
    ok = elvis_config:set_verbose(true),
    process_options(Opts, Cmds, Config);
process_options([version | Opts], Cmds, Config) ->
    version(),
    process_options(Opts, Cmds, Config);
process_options([{code_path, Path} | Opts], Cmds, Config) ->
    true = code:add_path(Path),
    process_options(Opts, Cmds, Config);
process_options([{parallel, Num} | Opts], Cmds, Config) ->
    N =
        case Num of
            "auto" ->
                erlang:system_info(schedulers);
            _ ->
                erlang:list_to_integer(Num)
        end,
    ok = elvis_config:set_parallel(N),
    process_options(Opts, Cmds, Config);
process_options([{warnings_as_errors, Choice} | Opts], Cmds, Config) ->
    ok = elvis_config:set_warnings_as_errors(Choice),
    process_options(Opts, Cmds, Config);
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config).

%% @private
-spec process_commands(
    [
        rock
        | help
        | string()
    ],
    [elvis_config:t()]
) ->
    ok.
process_commands([rock], Config) ->
    case elvis_core:rock(Config) of
        {errors, _} -> elvis_utils:erlang_halt(1);
        {warnings, _} -> elvis_utils:erlang_halt(0);
        ok -> ok
    end;
process_commands([rock | Files], Config) ->
    Paths = lists:map(fun file_to_path/1, Files),
    NewConfig = elvis_config:resolve_files(Config, Paths),
    case elvis_core:rock(NewConfig) of
        {errors, _} -> elvis_utils:erlang_halt(1);
        {warnings, _} -> elvis_utils:erlang_halt(0);
        ok -> ok
    end;
process_commands([help | Cmds], Config) ->
    Config = help(Config),
    process_commands(Cmds, Config);
process_commands([], _Config) ->
    ok;
process_commands([_ | _] = Cmds, _Config) ->
    error({unrecognized_or_unimplemented_command, Cmds}).

file_to_path(File) ->
    Path = atom_to_list(File),
    Dirname = filename:dirname(Path),
    Filename = filename:basename(Path),
    case elvis_file:find_files([Dirname], Filename) of
        [] -> error({enoent, Path});
        [File0] -> File0
    end.

%%% Options

%% @private
-spec help() -> ok.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "elvis", standard_io).

%% @private
-spec help([elvis_config:t()]) -> [elvis_config:t()].
help(Config) ->
    help(),
    Config.

%% @private
-spec commands() -> ok.
commands() ->
    Commands =
        <<
            "Elvis will do the following things for you when asked nicely:\n"
            "\n"
            "rock [file...] Rock your socks off by running all rules to your source files.\n"
        >>,
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
