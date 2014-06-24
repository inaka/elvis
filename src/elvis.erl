-module(elvis).
-behavior(application).

%% Public API

-export([
         rock/0,
         rock/1
        ]).

%% Application Behavior Callbacks
-export([
         start/2,
         stop/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rock() -> ok.
rock() ->
    rock(application:get_all_env(elvis)).

-spec rock(config()) -> ok.
rock(Config) ->
    run(Config),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application behavior callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(kernel:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), any()} | {error, atom()}.
start(_StartType, _StartArgs) ->
    {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec run(config()) -> ok.
run(Config) ->
    SrcDirs = elvis_utils:source_dirs(Config),
    Filename = "*.erl",
    Fun = fun(Path) ->
            elvis_utils:find_file(Path, Filename)
          end,
    FilePaths = lists:flatmap(Fun, SrcDirs),
    Results = lists:map(fun (Path) -> apply_rules(Config, Path) end, FilePaths),

    elvis_result:print(Results),
    ok.

apply_rules(Config, FilePath) ->
    Rules = elvis_utils:rules(Config),
    Acc = {[], Config, FilePath},
    {RuleResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),
    elvis_result:new(file, FilePath, RuleResults).

apply_rule({Function, Args}, Acc) ->
    apply_rule({elvis_rules, Function, Args}, Acc);
apply_rule({Module, Function, Args}, {Result, Config, FilePath}) ->
    Results = Module:Function(Config, FilePath, Args),
    RuleResult = elvis_result:new(rule, Function, Results),
    {[RuleResult | Result], Config, FilePath}.
