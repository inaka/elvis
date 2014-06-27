-module(elvis_git).

-export([
         staged_files/0,
         staged_content/1
        ]).

-define(LIST_STAGED,
        "git diff --name-only --staged").

-define(STAGED_CONTENT(Path),
        ["git show :", Path]).

-type file() :: {Path :: string(), Contents :: string()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec staged_files() -> [file()].
staged_files() ->
    Cmd = ?LIST_STAGED,
    Output = list_to_binary(os:cmd(Cmd)),

    Lines = binary:split(Output, <<"\n">>, [global]),
    Paths = lists:filter(fun(X) -> byte_size(X) > 0  end, Lines),

    lists:map(fun staged_content/1, Paths).

-spec staged_content(string()) -> file().
staged_content(Path) ->
    Content = os:cmd(?STAGED_CONTENT(Path)),
    {Path, Content}.
