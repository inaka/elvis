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

%%% Commands

-spec rock() -> ok.
rock() ->
    Config = elvis_config:default(),
    rock(Config).

-spec rock(elvis_config:config()) -> ok.
rock(Config) ->
    case elvis_config:validate(Config) of
        valid ->
            run(Config);
        invalid ->
            throw(invalid_config)
    end.

-spec git_hook(elvis_config:config()) -> ok.
git_hook(Config) ->
    Files = elvis_git:staged_files(),

    NewConfig = Config#{files => Files},

    case run(NewConfig) of
        fail -> halt(1);
        ok -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rocking it hard

-spec run(elvis_config:config()) -> ok | fail.
run(Config = #{files := Files}) ->
    Results = [apply_rules(Config, File) || File <- Files],

    elvis_result:print(Results),
    elvis_result:status(Results);
run(Config = #{src_dirs := SrcDirs}) ->
    Pattern = "*.erl",
    Files = elvis_utils:find_files(SrcDirs, Pattern),

    run(Config#{files => Files});
run(Config) ->
    throw({invalid_config, Config}).

-spec apply_rules(elvis_config:config(), elvis_utils:file()) ->
    elvis_result:file_result().
apply_rules(Config = #{rules := Rules}, File = #{path := Path}) ->
    Acc = {[], Config, File},
    {RuleResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),

    elvis_result:new(file, Path, RuleResults).

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
    rock(Config),
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
