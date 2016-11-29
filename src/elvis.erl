-module(elvis).

%% Public API

-export([main/1]).

-export([start/0]).

-define(APP_NAME, "elvis").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis_shell),
    ok.

-spec main(string()) -> ok.
main(Args) ->
    %% Load the application to be able to access its information
    %% (e.g. --version option)
    ok =
      case application:load(elvis) of
        ok -> ok;
        {error, {already_loaded, elvis}} -> ok
      end,
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[], []}} ->
            help();
        {ok, {Options, Commands}} ->
            process_options(Options, Commands);
        {error, {Reason, Data}} ->
            elvis_utils:error_prn("~s ~p~n", [Reason, Data]),
            help()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Command Line Interface

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Commands = "Provide the path to the configuration file. "
               ++ "When none is provided elvis checks if there's "
               ++ "an elvis.config file.",
    OutputFormat = "It allows you to display the results in plain text. When "
                   ++ "none is provided elvis displays the results in colors. "
                   ++ "The options allowed are (plain | colors).",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Commands},
     {commands, undefined, "commands", undefined, "Show available commands."},
     {output_format, undefined, "output-format", string, OutputFormat},
     {version, $v, "version", undefined, "Specify the elvis current version."},
     {code_path, $p, "code-path", string, "Add the directory in the code path."}
    ].

-spec process_options([atom()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = elvis_config:default(),
        AtomCommands = lists:map(fun list_to_atom/1, Commands),
        process_options(Options, AtomCommands, Config)
    catch
        throw:Exception ->
            elvis_utils:error_prn("~p.", [Exception])
    end.

-spec process_options([atom()], [string()], elvis_config:config()) ->
  ok.
process_options([help | Opts], Cmds, Config) ->
    help(),
    process_options(Opts, Cmds, Config);
process_options([{config, Path} | Opts], Cmds, _) ->
    Config = elvis_config:load_file(Path),
    process_options(Opts, Cmds, Config);
process_options([commands | Opts], Cmds, Config) ->
    commands(),
    process_options(Opts, Cmds, Config);
process_options([{output_format, Format} | Opts], Cmds, Config) ->
    ok = application:set_env(elvis, output_format, list_to_atom(Format)),
    process_options(Opts, Cmds, Config);
process_options([version | Opts], Cmds, Config) ->
    version(),
    process_options(Opts, Cmds, Config);
process_options([{code_path, Path} | Opts], Cmds, Config) ->
    true = code:add_path(Path),
    process_options(Opts, Cmds, Config);
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config).

-spec process_commands([string()], elvis_config:config()) ->ok.
process_commands([rock | Files], Config) ->
    case Files of
        [] ->
            case elvis_core:rock(Config) of
                {fail, _} -> elvis_utils:erlang_halt(1);
                ok -> ok
            end;
        _ ->
            lists:map(fun(F) -> rock_one_song(F, Config) end, Files)
    end;
process_commands([help | Cmds], Config) ->
    Config = help(Config),
    process_commands(Cmds, Config);
process_commands(['install', 'git-hook' | Cmds], Config) ->
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
    throw(unrecognized_or_unimplemented_command).

%%% Options

-spec help() -> ok.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, ?APP_NAME, standard_io).

-spec help(elvis_config:config()) ->
    elvis_config:config().
help(Config) ->
    help(),
    Config.

-spec commands() -> ok.
commands() ->
    Commands = <<"Elvis will do the following things for you when asked nicely:

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

-spec version() -> ok.
version() ->
    {ok, AppConfig} = application:get_all_key(elvis),
    Vsn = proplists:get_value(vsn, AppConfig),
    Version = "   ______     _   \n"
              "  / __/ /  __(_)__\n"
              " / _// / |/ / (_-<\n"
              "/___/_/|___/_/___/\n"
              "Version: ~s\n",
    io:format(Version, [Vsn]).


rock_one_song(FileName, Config) ->
    F = atom_to_list(FileName),
    case elvis_core:rock_this(F, Config) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end.
