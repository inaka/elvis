-module(elvis_meta_SUITE).

-export([all/0]).
-export([elvis/1]).

-type config() :: proplists:proplist().

-export_type([config/0]).

-hank([unnecessary_function_arguments]).

-spec all() -> [elvis, ...].
all() ->
    [elvis].

-spec elvis(config()) -> {comment, []}.
elvis(_Config) ->
    BaseDir = code:lib_dir(elvis),
    ConfigFile = filename:join(BaseDir, "../../../../elvis.config"),
    ElvisConfig = [fix_dirs(Group) || Group <- elvis_config:from_file(ConfigFile)],
    ct:comment("Elvis rocks!"),
    ok = elvis_core:rock(ElvisConfig),
    {comment, ""}.

fix_dirs(#{dirs := Dirs} = Group) ->
    NewDirs =
        [filename:join(
             code:lib_dir(elvis), Dir)
         || Dir <- Dirs],
    Group#{dirs := NewDirs}.
