-module(elvis).

%% Public API

-export([
         main/1
        ]).

-export([
         rock/0,
         rock/1,
         webhook/1
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
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            help()
    end.

%%% Rock Command

-spec rock() -> ok | {fail, elvis_result:file()}.
rock() ->
    Config = elvis_config:default(),
    rock(Config).

-spec rock(elvis_config:config()) -> ok | {fail, elvis_result:file()}.
rock(Config = #{files := Files, rules := _Rules}) ->
    Results = [apply_rules(Config, File) || File <- Files],

    elvis_result:print(Results),
    case elvis_result:status(Results) of
        fail -> {fail, Results};
        ok -> ok
    end;
rock(Config = #{src_dirs := SrcDirs, rules := _Rules}) ->
    Files = elvis_utils:find_files(SrcDirs),

    rock(Config#{files => Files});
rock(Config) ->
    throw({invalid_config, Config}).

%%% Git-Hook Command

-spec git_hook(elvis_config:config()) -> ok.
git_hook(Config) ->
    Files = elvis_git:staged_files(),
    ErlFiles = elvis_utils:filter_files(Files),

    NewConfig = Config#{files => ErlFiles},

    case rock(NewConfig) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end.

%% @doc Should receive the payload of a Github event and act accordingly.
-spec webhook(webhook:request()) -> ok | {error, term()}.
webhook(Request) ->
    elvis_webhook:event(Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec apply_rules(elvis_config:config(), elvis_utils:file()) ->
    elvis_result:file().
apply_rules(Config = #{rules := Rules}, File) ->
    Acc = {[], Config, File},
    {RulesResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),

    elvis_result:new(file, File, RulesResults).

apply_rule({Module, Function, Args}, {Result, Config, FilePath}) ->
    Results = Module:Function(Config, FilePath, Args),
    RuleResult = elvis_result:new(rule, Function, Results),

    {[RuleResult | Result], Config, FilePath}.

%%% Command Line Interface

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Commands = "Provide the path to the configuration file. "
               ++ "When none is provided elvis checks if there's "
               ++ "an elvis.config file.",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Commands},
     {commands, undefined, "commands", undefined, "Show available commands."} %% Long Line
    ].

-spec process_options([atom()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = elvis_config:default(),
        AtomCommands = lists:map(fun list_to_atom/1, Commands),
        process_options(Options, AtomCommands, Config)
    catch
        throw:Exception ->
            io:format("Error: ~p.~n", [Exception])
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
