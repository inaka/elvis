-module(elvis_meta_SUITE).

-export([all/0]).
-export([dialyzer/1, xref/1, elvis/1]).

-type config() :: proplists:proplist().

-export_type([config/0]).

-spec all() -> [dialyzer | xref | elvis].
all() ->
    [dialyzer, xref, elvis].

-spec dialyzer(config()) -> {comment, []}.
dialyzer(_Config) ->
    BaseDir = code:lib_dir(elvis),
    DefaultRebar3PltLoc = filename:join(BaseDir, "../../../default"),
    Plts =
        filelib:wildcard(
            filename:join(DefaultRebar3PltLoc, "*_plt")),
    Dirs = [filename:join(BaseDir, Dir) || Dir <- ["ebin", "test"]],
    Warnings = [error_handling, no_return, unmatched_returns],
    ct:comment("Dialyzer must emit no warnings"),
    Opts =
        [{analysis_type, succ_typings},
         {plts, Plts},
         {files_rec, Dirs},
         {check_plt, true},
         {warnings, Warnings}],
    [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
    {comment, ""}.

-spec xref(config()) -> {comment, []}.
xref(_Config) ->
    BaseDir = code:lib_dir(elvis),
    Dirs = [filename:join(BaseDir, Dir) || Dir <- ["ebin", "test"]],
    XrefConfig =
        #{dirs => Dirs, xref_defaults => [{verbose, true}, {recurse, true}, {builtins, true}]},
    Checks = [undefined_function_calls, locals_not_used, deprecated_function_calls],
    ct:comment("There are no Warnings"),
    [] = [Warning || Check <- Checks, Warning <- xref_runner:check(Check, XrefConfig)],
    {comment, ""}.

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
