-module(elvis_config).

-export([
         default/0,
         validate/1,
         load_file/1
        ]).

-export_type([
              config/0
             ]).

-type config() :: map().

-define(DEFAULT_CONFIG_PATH, "./elvis.config").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default() -> config().
default() ->
    case file:consult(?DEFAULT_CONFIG_PATH) of
        {ok, [Config]} ->
            load(Config);
        {error, enoent} ->
            application:get_env(elvis, config, #{});
        {error, Reason} ->
            throw(Reason)
    end.

%% @doc Checks that the configuration has all the required keys.
-spec validate(config()) -> valid | invalid.
validate(Config) ->
    RequiredKeys = [src_dirs, rules],
    Fun = fun(Key) -> maps:is_key(Key, Config) end,
    case lists:all(Fun, RequiredKeys) of
        true -> valid;
        false -> invalid
    end.

-spec load_file(string()) -> config().
load_file(Path) ->
    case file:consult(Path) of
        {ok, [Config]} ->
            load(Config);
        {error, Reason} ->
            throw(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(AppConfig) ->
    ElvisConfig = proplists:get_value(elvis, AppConfig, []),
    proplists:get_value(config, ElvisConfig, #{}).
