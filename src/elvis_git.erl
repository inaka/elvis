-module(elvis_git).

-export([
         staged_files/0,
         staged_content/1
        ]).

-define(LIST_STAGED,
        "git diff --name-only --staged").

-define(STAGED_CONTENT(Path),
        "git show :" ++ Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec staged_files() -> [elvis_utils:file()].
staged_files() ->
    Cmd = ?LIST_STAGED,
    Output = list_to_binary(os:cmd(Cmd)),

    Lines = binary:split(Output, <<"\n">>, [global]),
    Paths = [binary_to_list(Path) || Path <- Lines, byte_size(Path) > 0],

    lists:map(fun staged_content/1, Paths).

-spec staged_content(string()) -> elvis_utils:file().
staged_content(Path) ->
    Content = os:cmd(?STAGED_CONTENT(Path)),
    #{
       path => Path,
       content => list_to_binary(Content)
     }.
