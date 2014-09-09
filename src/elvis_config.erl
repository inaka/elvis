-module(elvis_config).

-export([
         default/0,
         load_file/1,
         load/1,
         dirs/1
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
    proplists:get_value(config, ElvisConfig, #{}).

-spec dirs(config()) -> [string()].
dirs(#{dirs := Dirs}) ->
    Dirs;
dirs(#{}) ->
    [].
