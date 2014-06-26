-module(elvis).

%% Public API

-export([
         rock/0,
         rock/1
        ]).

-export_type([
              config/0
             ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rock() -> ok.
rock() ->
    Config = application:get_all_env(elvis),
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
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
