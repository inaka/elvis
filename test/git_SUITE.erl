-module(git_SUITE).

-export([
         all/0
        ]).

-export([
         relative_position_from_patch/1
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

    {ok, 5} = elvis_git:relative_position(Patch, 112),
    {ok, 14} = elvis_git:relative_position(Patch, 179).
