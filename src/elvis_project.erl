-module(elvis_project).

-export([
         no_deps_master_erlang_mk/3,
         no_deps_master_rebar/3
        ]).

-define(DEP_MASTER,
        "Dependency '~s' revision is specified 'master', "
        "please change this to a tag, branch or specific "
        "commit.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec no_deps_master_erlang_mk(elvis_config:config(),
                               elvis_utils:file(),
                               [term()]) ->
    [elvis_result:item()].
no_deps_master_erlang_mk(_Config, Target, []) ->
    Deps = get_erlang_mk_deps(Target),
    DepsInMaster = lists:filter(fun is_erlang_mk_master_dep/1, Deps),
    DepToResult = fun(Line) ->
                      Opts = [{capture, all_but_first, binary}],
                      {match, [Name]} = re:run(Line, "dep_([^ ]*)", Opts),
                      elvis_result:new(item, ?DEP_MASTER, [Name])
                  end,

    lists:map(DepToResult, DepsInMaster).

-spec no_deps_master_rebar(elvis_config:config(),
                           elvis_utils:file(),
                           [term()]) ->
    [elvis_result:item()].
no_deps_master_rebar(_Config, Target, []) ->
    Deps = get_rebar_deps(Target),
    DepsInMaster = lists:filter(fun is_rebar_master_dep/1, Deps),

    DepToResult = fun({AppName, _, _}) ->
                          elvis_result:new(item, ?DEP_MASTER, [AppName])
                  end,

    lists:map(DepToResult, DepsInMaster).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

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
