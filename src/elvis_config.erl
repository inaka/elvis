-module(elvis_config).

-export([
         default/0,
         load_file/1,
         load/1,
         validate/1,
         normalize/1,
         %% Geters
         dirs/1,
         filter/1,
         files/1,
         rules/1,
         %% Files
         resolve_files/1,
         resolve_files/2,
         apply_to_files/2
        ]).

-export_type([
              config/0
             ]).

-type config() :: [map()].

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

-spec validate(config()) -> ok | {error, term()}.
validate([]) ->
    throw({invalid_config, empty_config});
validate(Config) when is_list(Config) ->
    lists:foreach(fun validate/1, Config);
validate(RuleGroup) ->
    case maps:is_key(src_dirs, RuleGroup) or maps:is_key(dirs, RuleGroup) of
        false -> throw({invalid_config, {missing_dirs, RuleGroup}});
        true -> ok
    end,
    case maps:is_key(dirs, RuleGroup) of
        true ->
            case maps:is_key(filter, RuleGroup) of
                false -> throw({invalid_config, {missing_filter, RuleGroup}});
                true -> ok
            end;
        false -> ok
    end,
    case maps:is_key(rules, RuleGroup) of
        false -> throw({invalid_config, {missing_rules, RuleGroup}});
        true -> ok
    end.

-spec normalize(config()) -> config().
normalize(Config) when is_list(Config) ->
    lists:map(fun normalize/1, Config);
normalize(Config = #{src_dirs := Dirs}) ->
    %% NOTE: Provided for backwards compatibility.
    %% Rename 'src_dirs' key to 'dirs'.
    Config1 = maps:remove(src_dirs, Config),
    Config1#{dirs => Dirs};
normalize(Config) ->
    Config.

-spec dirs(config()) -> [string()].
dirs(Config) when is_list(Config) ->
    lists:flatmap(fun dirs/1, Config);
dirs(_RuleGroup = #{dirs := Dirs}) ->
    Dirs;
dirs(#{}) ->
    [].

-spec filter(config()) -> [string()].
filter(_RuleGroup = #{filter := Filter}) ->
    Filter;
filter(#{}) ->
    ?DEFAULT_FILTER.

-spec files(config()) -> [string()].
files(_RuleGroup = #{files := Files}) ->
    Files;
files(#{}) ->
    undefined.

-spec rules(config()) -> [string()] | undefined.
rules(_RuleGroup = #{rules := Rules}) ->
    Rules;
rules(#{}) ->
    undefined.

%% @doc Takes a configuration and a list of files, filtering some
%%      of them according to the 'filter' key, or if not specified
%%      uses '*.erl'.
%% @end
-spec resolve_files(config(), [elvis_file:file()]) -> config().
resolve_files(Config, Files) when is_list(Config) ->
    Fun = fun(RuleGroup) -> resolve_files(RuleGroup, Files) end,
    lists:map(Fun, Config);
resolve_files(RuleGroup, Files) ->
    Filter = filter(RuleGroup),
    Dirs = dirs(RuleGroup),
    FilteredFiles = elvis_file:filter_files(Files, Dirs, Filter),
    RuleGroup#{files => FilteredFiles}.

%% @doc Takes a configuration and finds all files according to its 'dirs'
%%      end  'filter' key, or if not specified uses '*.erl'.
%% @end
-spec resolve_files(config()) -> config().
resolve_files(Config) when is_list(Config) ->
    lists:map(fun resolve_files/1, Config);
resolve_files(RuleGroup = #{files := _Files}) ->
    RuleGroup;
resolve_files(RuleGroup = #{dirs := Dirs, filter := Filter}) ->
    Files = elvis_file:find_files(Dirs, Filter, local),
    RuleGroup#{files => Files};
resolve_files(RuleGroup = #{dirs := Dirs}) ->
    Files = elvis_file:find_files(Dirs, ?DEFAULT_FILTER),
    RuleGroup#{files => Files}.

%% @doc Takes a function and configuration and applies the function to all
%%      file in the configuration.
%% @end
-spec apply_to_files(fun(), config()) -> config().
apply_to_files(Fun, Config) when is_list(Config) ->
    ApplyFun = fun(RuleGroup) -> apply_to_files(Fun, RuleGroup) end,
    lists:map(ApplyFun, Config);
apply_to_files(Fun, RuleGroup = #{files := Files}) ->
    NewFiles = lists:map(Fun, Files),
    RuleGroup#{files => NewFiles}.
