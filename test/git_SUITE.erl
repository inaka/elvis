-module(git_SUITE).

-export([
         all/0
        ]).

-export([
         relative_position_from_patch/1,
         check_staged_files/1
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
    Module = ?MODULE,
    Exports = Module:module_info(exports),
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
    Filename = "../../temp_file_test",
    file:write_file(Filename, <<"sdsds">>, [append]),

    os:cmd("git add " ++ Filename),
    [#{path := "temp_file_test"}] = elvis_git:staged_files(),
    os:cmd("git reset " ++ Filename),

    file:delete(Filename).
