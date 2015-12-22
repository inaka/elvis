-module(elvis).

%% Public API

-export([
         main/1
        ]).

-export([
         rock/0,
         rock/1,
         rock_this/1,
         rock_this/2,
         webhook/1,
         webhook/2
        ]).

-export([start/0]).

-define(APP_NAME, "elvis").

-type source_filename() :: nonempty_string().
-type target() :: source_filename() | module().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis),
    ok.

-spec main([string()]) -> ok.
main(Args) ->
    %% Load the application to be able to access its information
    %% (e.g. --version option)
    application:load(elvis),
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

%%% Rock Command

-spec rock() -> ok | {fail, [elvis_result:file()]}.
rock() -> elvis_core:rock().

-spec rock(elvis_config:config()) -> ok | {fail, [elvis_result:file()]}.
rock(Config) -> elvis_core:rock(Config).

-spec rock_this(target()) ->
    ok | {fail, elvis_result:file()}.
rock_this(Target) -> elvis_core:rock_this(Target).

-spec rock_this(target(), elvis_config:config()) ->
    ok | {fail, elvis_result:file()}.
rock_this(Module, Config) -> elvis_core:rock_this(Module, Config).

%%% Git-Hook Command

-spec git_hook(elvis_config:config()) -> ok.
git_hook(Config) ->
    Files = elvis_git:staged_files(),
    NewConfig = elvis_config:resolve_files(Config, Files),

    case rock(NewConfig) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end.

%% @doc Should receive the payload of a Github event and act accordingly.
-spec webhook(webhook:request()) -> ok | {error, term()}.
webhook(Request) ->
    Credentials = github_credentials(),
    elvis_webhook:event(Credentials, Request).

%% @doc Receives github credentials (basic or OAuth) and the payload
%%      from an event, acting accordingly.
-spec webhook(egithub:credentials(), webhook:request()) ->
    ok | {error, term()}.
webhook(Credentials, Request) ->
    elvis_webhook:event(Credentials, Request).

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

-spec process_options([atom()], [string()], elvis_config:config()) -> ok.
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
    code:add_path(Path),
    process_options(Opts, Cmds, Config);
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config).

-spec process_commands([string()], elvis_config:config()) -> ok.
process_commands([rock | Cmds], Config) ->
    case rock(Config) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end,
    process_commands(Cmds, Config);
process_commands([help | Cmds], Config) ->
    Config = help(Config),
    process_commands(Cmds, Config);
process_commands(['install', 'git-hook' | Cmds], Config) ->
    elvis_git:install_hook(),
    process_commands(Cmds, Config);
process_commands(['git-hook' | Cmds], Config) ->
    git_hook(Config),
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

rock             Rock your socks off by running all rules to your source files.

git-hook         Pre-commit Git Hook: Gets all staged files and runs the rules
                                      specified in the configuration to those
                                      files.
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

-spec github_credentials() -> egithub:credentials().
github_credentials() ->
    User = application:get_env(elvis, github_user, ""),
    Password = application:get_env(elvis, github_password, ""),
    egithub:basic_auth(User, Password).
