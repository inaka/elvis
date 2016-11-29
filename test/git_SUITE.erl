-module(git_SUITE).

-export([
         all/0
        ]).

-export([
         relative_position_from_patch/1,
         check_staged_files/1,
         ignore_deleted_files/1,
         check_branch_files/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec relative_position_from_patch(config()) -> any().
relative_position_from_patch(_Config) ->
    Patch = <<"@@ -109,7 +109,7 @@ option_spec_list() ->\n     [\n      {help,"
              " $h, \"help\", undefined, \"Show this help information.\"},\n"
              "      {config, $c, \"config\", string, Commands},\n-     "
              "{commands, undefined, \"commands\", undefined, \"Show available"
              " commands.\"}\n+     {commands, undefined, \"commands\", "
              "undefined, \"Show available commands.\"} %% Long Line\n    "
              " ].\n \n -spec process_options([atom()], [string()]) -> ok."
              "\n@@ -175,3 +175,5 @@ git-hook         Pre-commit Git Hook: "
              "Gets all staged files and runs the rules\n                  "
              "                     files.\n \">>,\n    io:put_chars(Commands)."
              "\n+\n+%% Another dummy change to check how patches are built "
              "with changes wide apart.">>,

    {ok, 1} = elvis_git:relative_position(Patch, 109),
    {ok, 5} = elvis_git:relative_position(Patch, 112),
    {ok, 8} = elvis_git:relative_position(Patch, 115),

    {ok, 10} = elvis_git:relative_position(Patch, 175),
    {ok, 12} = elvis_git:relative_position(Patch, 177),
    {ok, 14} = elvis_git:relative_position(Patch, 179),

    not_found = elvis_git:relative_position(Patch, 108),
    not_found = elvis_git:relative_position(Patch, 116),

    not_found = elvis_git:relative_position(Patch, 174),
    not_found = elvis_git:relative_position(Patch, 180).


-spec check_staged_files(config()) -> any().
check_staged_files(_Config) ->
    FileName = random_file_name(),
    FileLocation = "../../" ++ FileName,
    ok = file:write_file(FileLocation, <<"sdsds">>, [append]),

    _ = os:cmd("git add " ++ FileLocation),
    StagedFiles = elvis_git:staged_files(),
    FilterFun = fun (#{path := Path}) -> Path == FileName end,
    [_] = lists:filter(FilterFun, StagedFiles),
    _ = os:cmd("git reset " ++ FileLocation),

    file:delete(FileLocation).

-spec ignore_deleted_files(config()) -> any().
ignore_deleted_files(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(PrivDir), %ct will set cwd of each test to its log_dir
    _ = os:cmd("git init ."),
    ok = file:write_file("test", <<"ingore">>, [append]),
    ok = file:write_file("test2", <<"ignore">>, [append]),
    _ = os:cmd("git add . && git commit -m 'Add dummy files'"),
    _ = os:cmd("git rm test"),
    ok = file:write_file("test2", <<"ignore">>, [append]), %modified
    ok = file:write_file("test3", <<"ignore">>, [append]), %new
    _ = os:cmd("git add ."),
    StagedFiles = lists:sort([Path
                              || #{path := Path} <- elvis_git:staged_files()]),
    ["test2", "test3"] = StagedFiles.

-spec check_branch_files(config()) -> any().
check_branch_files(_Config) ->
    _ = os:cmd("git stash"),

    FileName = random_file_name(),
    FileLocation = "../../" ++ FileName,
    ok = file:write_file(FileLocation, <<"sdsds">>, [append]),

    _ = os:cmd("git add " ++ FileLocation),
    _ = os:cmd("git commit -m \"some commit\""),
    [FileName] = elvis_git:branch_files("HEAD^"),
    _ = os:cmd("git reset --hard HEAD^"),

    _ = os:cmd("git stash pop"),

    file:delete(FileLocation).

-spec random_file_name() -> string().
random_file_name() ->
  RandomString = integer_to_list(crypto:rand_uniform(0, 1 bsl 127)),
  "test_file_" ++ RandomString.
