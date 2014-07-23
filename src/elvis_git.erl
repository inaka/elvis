-module(elvis_git).

-export([
         staged_files/0,
         staged_content/1,
         relative_position/2
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

%% @doc Takes a git patch and an absolute file line number and returns a
%%      relative position for that line in the patch.
-spec relative_position(binary(), integer()) ->
    {ok, integer()} | not_found.
relative_position(Patch, LineNum) ->
    Lines = binary:split(Patch, <<"\n">>, [global]),
    relative_position(Lines, LineNum, {-1, undefined}).

relative_position([], _Num, _Positions) ->
    not_found;
relative_position([Line | Lines], Num, Positions) ->
    Type = patch_line_type(Line),
    case new_position(Line, Positions) of
        {NewLocal, NewGlobal} when
              NewGlobal == Num,
              Type =/= patch,
              Type =/= deletion ->
            {ok, NewLocal};
        NewPositions ->
            relative_position(Lines, Num, NewPositions)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Return the corresponding local and global line increments based
%%      on the line's type.
new_position(Line, {Local, Global}) ->
    case patch_line_type(Line) of
        patch ->
            NewGlobal = patch_position(Line),
            {Local + 1, NewGlobal - 1};
        deletion ->
            {Local + 1, Global};
        addition ->
            {Local + 1, Global + 1};
        same ->
            {Local + 1, Global + 1}
    end.

%% @private
%% @doc Takes a line form a git patch and returns its type.
-spec patch_line_type(binary()) -> patch | addition | deletion | same.
patch_line_type(Line) ->
    [Head | _] = elvis_utils:to_str(Line),
    case Head of
        $@ -> patch;
        $+ -> addition;
        $- -> deletion;
        $  -> same
    end.

%% @private
%% @doc Takes a patch type line and returns the line number after the +.
-spec patch_position(binary()) -> integer().
patch_position(Line) ->
    Regex = "^@@ .*? \\+(\\d+),.*$",
    Opts = [{capture, all_but_first, list}],
    {match, [PositionStr | _ ]} = re:run(Line, Regex, Opts),
    list_to_integer(PositionStr).
