-module(elvis_config).

-export([
         default/0,
         load_file/1,
         load/1,
         dirs/1,
         resolve_files/1,
         resolve_files/2,
         apply_to_files/2
        ]).

-export_type([
              config/0
             ]).

-type config() :: map().

-define(DEFAULT_CONFIG_PATH, "./elvis.config").
-define(DEFAULT_FILTER, "*.erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default() -> config().
default() ->
    case file:consult(?DEFAULT_CONFIG_PATH) of
        {ok, [Config]} ->
            load(Config);
        {error, enoent} ->
            Config = application:get_env(elvis, config, []),
            ensure_config_list(Config);
        {error, Reason} ->
            throw(Reason)
    end.

-spec load_file(string()) -> config().
load_file(Path) ->
    case file:consult(Path) of
        {ok, [Config]} ->
            load(Config);
        {error, Reason} ->
            throw(Reason)
    end.

-spec load(term()) -> config().
load(AppConfig) ->
    ElvisConfig = proplists:get_value(elvis, AppConfig, []),
    Config =  proplists:get_value(config, ElvisConfig, []),
    ensure_config_list(Config).

ensure_config_list(Config) when is_map(Config) ->
    [Config];
ensure_config_list(Config) ->
    Config.

-spec dirs(config()) -> [string()].
dirs(Config) when is_list(Config) ->
    lists:flatmap(fun dirs/1, Config);
dirs(_RuleGroup = #{dirs := Dirs}) ->
    Dirs;
dirs(#{}) ->
    [].

-spec resolve_files(config(), [elvis_utils:file()]) -> config().
resolve_files(Config, Files) when is_list(Config) ->
    Fun = fun(RuleGroup) -> resolve_files(RuleGroup, Files) end,
    lists:map(Fun, Config);
resolve_files(RuleGroup = #{filter := Filter}, Files) ->
    Filter = maps:get(filter, RuleGroup),
    FilteredFiles = elvis_utils:filter_files(Files, Filter),
    RuleGroup#{files => FilteredFiles};
resolve_files(RuleGroup, Files) ->
    FilteredFiles = elvis_utils:filter_files(Files, ?DEFAULT_FILTER),
    RuleGroup#{files => FilteredFiles}.

-spec resolve_files(config()) -> config().
resolve_files(Config) when is_list(Config) ->
    lists:map(fun resolve_files/1, Config);
resolve_files(RuleGroup = #{dirs := Dirs, filter := Filter}) ->
    Files = elvis_utils:find_files(Dirs, Filter, local),
    RuleGroup#{files => Files};
resolve_files(RuleGroup = #{dirs := Dirs}) ->
    Files = elvis_utils:find_files(Dirs, ?DEFAULT_FILTER),
    RuleGroup#{files => Files}.

-spec apply_to_files(fun(), config()) -> config().
apply_to_files(Fun, Config) when is_list(Config) ->
    ApplyFun = fun(RuleGroup) -> apply_to_files(Fun, RuleGroup) end,
    lists:map(ApplyFun, Config);
apply_to_files(Fun, RuleGroup = #{files := Files}) ->
    NewFiles = lists:map(Fun, Files),
    RuleGroup#{files => NewFiles}.
