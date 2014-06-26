-module(elvis).

%% Public API

-export([
         main/1
        ]).

-export([
         rock/0,
         rock/1
        ]).

-define(APP_NAME, "elvis").

-export_type([
              config/0
             ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-spec rock() -> ok.
rock() ->
    Config = default_config(),
    rock(Config).

-spec rock(config()) -> ok.
rock(Config) ->
    case elvis_utils:validate_config(Config) of
        valid ->
            run(Config);
        invalid ->
            throw(invalid_config)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rocking it hard

-spec run(config()) -> ok.
run(Config) ->
    SrcDirs = elvis_utils:source_dirs(Config),
    Pattern = "*.erl",
    FilePaths = elvis_utils:find_files(SrcDirs, Pattern),
    Results = lists:map(fun (Path) -> apply_rules(Config, Path) end, FilePaths),

    elvis_result:print(Results).

apply_rules(Config, FilePath) ->
    Rules = elvis_utils:rules(Config),
    Acc = {[], Config, FilePath},
    {RuleResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),
    elvis_result:new(file, FilePath, RuleResults).

apply_rule({Module, Function, Args}, {Result, Config, FilePath}) ->
    Results = Module:Function(Config, FilePath, Args),
    RuleResult = elvis_result:new(rule, Function, Results),
    {[RuleResult | Result], Config, FilePath}.


%%% Command Line Interface

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Commands = "Provide the path to the configuration file.",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Commands},
     {commands, undefined, "commands", undefined, "Show available commands."}
    ].

-spec process_options([atom()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = default_config(),
        process_options(Options, Commands, Config)
    catch
        throw:Exception ->
            io:format("Error: ~p.~n", [Exception])
    end.

-spec process_options([atom()], [string()], config()) -> ok.
process_options([help | Opts], Cmds, Config) ->
    help(),
    process_options(Opts, Cmds, Config);
process_options([{config, Path} | Opts], Cmds, _Config) ->
    Config = config(Path),
    process_options(Opts, Cmds, Config);
process_options([], Cmds, Config) ->
    process_commands(Cmds, Config);
process_options([_Opt | _Opts], _Cmds, _Config) ->
    throw(unrecognized_or_unimplemened_option).

-spec process_commands([string()], config()) -> ok.
process_commands(["rock" | Cmds], Config) ->
    rock(Config),
    process_commands(Cmds, Config);
process_commands(["help" | Cmds], Config) ->
    Config = help(Config),
    process_commands(Cmds, Config);
process_commands([], _Config) ->
    ok;
process_commands([_Cmd | _Cmds], _Config) ->
    throw(unrecognized_or_unimplemened_command).

-spec default_config() -> config().
default_config() ->
    case file:consult("./elvis.config") of
        {ok, [Config]} ->
            Config;
        {error, enoent} ->
            application:get_all_env(elvis);
        {error, Reason} ->
            throw(Reason)
    end.

%%% Options

-spec help() -> ok.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, ?APP_NAME).

-spec help(config()) -> config().
help(Config) ->
    help(),
    Config.

-spec config(string()) -> config().
config(Path) ->
    case file:consult(Path) of
        {ok, [Config]} ->
            Config;
        {error, Reason} ->
            throw(Reason)
    end.
