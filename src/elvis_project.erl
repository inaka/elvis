-module(elvis_project).

-export([no_deps_master/2]).

-define(DEP_MASTER,
        "Dependency '~s' revision is specified 'master', "
        "please change this to a tag, branch or specific "
        "commit.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec no_deps_master(elvis_config:config(), [term()]) ->
    [elvis_result:file()].
no_deps_master(_Config, []) ->
    RebarResult =
        case find_file(".", "rebar.config") of
            {ok, RebarFile} ->
                RebarRuleResult = [rebar_deps_master(RebarFile)],
                [elvis_result:new(file, RebarFile, RebarRuleResult)];
            {error, enoent} ->
                []
        end,

    ErlangMkResult =
        case find_file(".", "Makefile") of
            {ok, MakeFile} ->
                ErlangMkRuleResult = erlang_mk_deps_master(MakeFile),
                [elvis_result:new(file, MakeFile, ErlangMkRuleResult)];
            {error, enoent} ->
                []
        end,

    RebarResult ++  ErlangMkResult.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

-spec rebar_deps_master(elvis_utils:file()) -> elvis_result:rule().
rebar_deps_master(File) ->
    Deps = get_rebar_deps(File),
    DepsInMaster = lists:filter(fun is_rebar_master_dep/1, Deps),

    DepToResult = fun({AppName, _, _}) ->
                          elvis_result:new(item, ?DEP_MASTER, [AppName])
                  end,

    Results = lists:map(DepToResult, DepsInMaster),
    elvis_result:new(rule, no_deps_master, Results).

get_rebar_deps(File) ->
    Path = elvis_utils:path(File),
    {ok, Terms} = file:consult(Path),
    IsDepsTerm = fun
                     ({deps, _}) -> true;
                     (_) -> false
                 end,
    case lists:filter(IsDepsTerm, Terms) of
        [] -> [];
        [{deps, Deps}] -> Deps
    end.

is_rebar_master_dep({_AppName, _Vsn, {_SCM, _Location, "master"}}) ->
    true;
is_rebar_master_dep({_AppName, _Vsn, {_SCM, _Location, {branch, "master"}}}) ->
    true;
is_rebar_master_dep(_) ->
    false.

%%% erlang.mk

-spec erlang_mk_deps_master(elvis_utils:file()) -> elvis_result:rule().
erlang_mk_deps_master(File) ->
    Deps = get_erlang_mk_deps(File),
    DepsInMaster = lists:filter(fun is_erlang_mk_master_dep/1, Deps),
    DepToResult = fun(Line) ->
                      Opts = [{capture, all_but_first, binary}],
                      {match, [Name]} = re:run(Line, "dep_([^ ]*)", Opts),
                      elvis_result:new(item, ?DEP_MASTER, [Name])
                  end,

    Results = lists:map(DepToResult, DepsInMaster),
    elvis_result:new(rule, no_deps_master, Results).

is_erlang_mk_master_dep(Line) ->
    case re:run(Line, "master *$", []) of
        nomatch -> false;
        _ -> true
    end.

get_erlang_mk_deps(File) ->
    {Src, _} = elvis_utils:src(File),
    Lines = binary:split(Src, <<"\n">>, [global]),
    IsDepsLine = fun(Line) -> re:run(Line, "dep_", []) /= nomatch end,
    lists:filter(IsDepsLine, Lines).

find_file(Dir, Filename) ->
    case elvis_utils:find_files([Dir], Filename, local) of
        [] ->
            {error, enoent};
        [File] ->
            {ok, File}
    end.
