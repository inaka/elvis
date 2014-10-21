-module(elvis).

%% Public API

-export([
         main/1
        ]).

-export([
         rock/0,
         rock/1,
         webhook/1,
         webhook/2
        ]).

-export([start/0]).

-define(APP_NAME, "elvis").

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
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[], []}} ->
            help();
        {ok, {Options, Commands}} ->
            process_options(Options, Commands);
        {error, {Reason, Data}} ->
            elvis_utils:error_prn("~s ~p~n~n", [Reason, Data]),
            help()
    end.

%%% Rock Command

-spec rock() -> ok | {fail, elvis_result:file()}.
rock() ->
    Config = elvis_config:default(),
    rock(Config).

-spec rock(elvis_config:config()) -> ok | {fail, [elvis_result:file()]}.
rock(Config) ->
    elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
    Results = lists:map(fun do_rock/1, NewConfig),
    lists:foldl(fun combine_results/2, ok, Results).

%% @private
-spec do_rock(elvis_config:config()) -> ok | {fail, [elvis_result:file()]}.
do_rock(Config0) ->
    elvis_utils:info("Loading files..."),
    Config = elvis_config:resolve_files(Config0),
    Files = elvis_config:files(Config),
    Fun = fun (File) -> load_file_data(Config, File) end,
    LoadedFiles = lists:map(Fun, Files),
    elvis_utils:info("Applying rules..."),
    Results = [apply_rules(Config, File) || File <- LoadedFiles],

    case elvis_result:status(Results) of
        fail -> {fail, Results};
        ok -> ok
    end.

%% @private
-spec load_file_data(elvis_config:config(), elvis_file:file()) ->
    elvis_file:file().
load_file_data(Config, File) ->
    Path = elvis_file:path(File),
    elvis_utils:info("Loading ~s", [Path]),
    try
        elvis_file:load_file_data(Config, File)
    catch
        _:Reason ->
            Msg = "~p when loading file ~p.",
            elvis_utils:error_prn(Msg, [Reason, Path]),
            File
    end.

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

-spec combine_results(ok | {fail, [elvis_result:file()]},
                      ok | {fail, [elvis_result:file()]}) ->
    ok | {fail, [elvis_result:file()]}.
combine_results(ok, Acc) ->
    Acc;
combine_results(Item, ok) ->
    Item;
combine_results({fail, ItemResults}, {fail, AccResults}) ->
    {fail, ItemResults ++ AccResults}.

-spec apply_rules(elvis_config:config(), elvis_file:file()) ->
    elvis_result:file().
apply_rules(Config, File) ->
    Rules = elvis_config:rules(Config),
    Acc = {[], Config, File},
    {RulesResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),

    Results = elvis_result:new(file, File, RulesResults),
    elvis_result:print(Results),
    Results.

apply_rule({Module, Function, Args}, {Result, Config, File}) ->
    RuleResult = try
                     Results = Module:Function(Config, File, Args),
                     elvis_result:new(rule, Function, Results)
                 catch
                     _:Reason ->
                         Msg = "'~p' while applying rule '~p'.",
                         elvis_result:new(error, Msg, [Reason, Function])
                 end,
    {[RuleResult | Result], Config, File}.

%%% Command Line Interface

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Commands = "Provide the path to the configuration file. "
               ++ "When none is provided elvis checks if there's "
               ++ "an elvis.config file.",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Commands},
     {commands, undefined, "commands", undefined, "Show available commands."}
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
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config).

-spec process_commands([string()], elvis_config:config()) -> ok.
process_commands([rock | Cmds], Config) ->
    _ = rock(Config),
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
    throw(unrecognized_or_unimplemened_command).

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
">>,
   io:put_chars(Commands).

-spec github_credentials() -> egithub:credentials().
github_credentials() ->
    User = application:get_env(elvis, github_user, ""),
    Password = application:get_env(elvis, github_password, ""),
    egithub:basic_auth(User, Password).
