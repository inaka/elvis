-module(elvis_git).

-export([
         run_hook/1,
         run_branch/2,
         staged_files/0,
         branch_files/1,
         staged_content/1,
         relative_position/2,
         install_hook/0
        ]).

-define(LIST_STAGED,
        "git diff --name-status --staged | awk '$1 != \"D\" { print $2 }'").

-define(LIST_BRANCH_CHANGES(C),
        "git diff --name-only --ignore-submodules=all --diff-filter=d " ++ C).

-define(STAGED_CONTENT(Path),
        "git show :" ++ Path).

-define(PRE_COMMIT_FILE, ".git/hooks/pre-commit").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec run_hook(elvis_config:config()) -> ok.
run_hook(Config) ->
    Files = elvis_git:staged_files(),
    NewConfig = elvis_config:resolve_files(Config, Files),
    case elvis_core:rock(NewConfig) of
        {fail, _} -> elvis_utils:erlang_halt(1);
        ok -> ok
    end.

-spec run_branch(string(), elvis_config:config()) -> ok.
run_branch(Commit, Config) ->
    Files = elvis_git:branch_files(Commit),
    Results = lists:map(fun(File) ->
                                elvis_core:rock_this(File, Config)
                        end, Files),
    case lists:any(fun(Res) -> Res /= ok end, Results) of
        true -> elvis_utils:erlang_halt(1);
        false -> ok
    end.

-spec branch_files(string()) -> [elvis_file:file()].
branch_files(Commit) ->
    Cmd = ?LIST_BRANCH_CHANGES(Commit),
    Output = list_to_binary(os:cmd(Cmd)),

    Lines = binary:split(Output, <<"\n">>, [global]),
    [binary_to_list(Path) || Path <- Lines, byte_size(Path) > 0].

-spec staged_files() -> [elvis_file:file()].
staged_files() ->
    Cmd = ?LIST_STAGED,
    Output = list_to_binary(os:cmd(Cmd)),

    Lines = binary:split(Output, <<"\n">>, [global]),
    Paths = [binary_to_list(Path) || Path <- Lines, byte_size(Path) > 0],

    lists:map(fun staged_content/1, Paths).

-spec staged_content(string()) -> elvis_file:file().
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

%% @doc Install a pre commit hook for the git repository
%%      in the current dir.
-spec install_hook() -> ok.
install_hook() ->
    try
        check_git_dir(),
        ok = filelib:ensure_dir(?PRE_COMMIT_FILE),
        _ = add_pre_commit_hook(),
        elvis_utils:info("Elvis pre-commit hook installed. "
                         "Wop-bop-a-loom-a-blop-bam-boom!")
    catch
        _:Reason ->
            elvis_utils:error_prn(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Check if the current dir is a git repository.
check_git_dir() ->
    case filelib:is_dir(".git") of
        true -> ok;
        false -> throw("Not a git repository.")
    end.

%% @doc Adds elvis as a pre commit hook. If a pre-commit file already exists
%%      appends the command to it, otherwise the file is created.
add_pre_commit_hook() ->
    Filename = ?PRE_COMMIT_FILE,

    Header = <<"#!/bin/sh\n">>,
    Command = <<"elvis git-hook\n">>,

    {Mode, Data} =
        case filelib:is_file(Filename) of
            true ->
                {ok, Content} = file:read_file(?PRE_COMMIT_FILE),
                case binary:match(Content, <<"elvis">>) of
                    nomatch -> {[append], Command};
                    _ ->  throw("Elvis is already installed as a git hook.")
                end;
            false -> {[write], <<Header/binary, Command/binary>>}
        end,

    ok = file:write_file(Filename, Data, Mode),
    os:cmd("chmod +x " ++ ?PRE_COMMIT_FILE).

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
        _ -> %% addition or same
            {Local + 1, Global + 1}
    end.

%% @private
%% @doc Takes a line form a git patch and returns its type.
-spec patch_line_type(binary()) -> patch | addition | deletion | same.
patch_line_type(Line) ->
    [Head | _] = elvis_utils:to_str(Line),
    case Head of
        $@  -> patch;
        $+  -> addition;
        $-  -> deletion;
        $\\ -> same;
        $   -> same %space
    end.

%% @private
%% @doc Takes a patch type line and returns the line number after the +.
-spec patch_position(binary()) -> integer().
patch_position(Line) ->
    Regex = "^@@ .*? \\+(\\d+),.*$",
    Opts = [{capture, all_but_first, list}],
    {match, [PositionStr | _ ]} = re:run(Line, Regex, Opts),
    list_to_integer(PositionStr).
