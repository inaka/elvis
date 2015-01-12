-module(elvis_project).

-export([
         no_deps_master_erlang_mk/3,
         no_deps_master_rebar/3,
         git_for_deps_erlang_mk/3,
         git_for_deps_rebar/3,
         old_configuration_format/3
        ]).

-define(DEP_MASTER,
        "Dependency '~s' revision is specified 'master', "
        "please change this to a tag, branch or specific "
        "commit.").

-define(DEP_NO_GIT,
        "Dependency '~s' is not using git protocol, "
        "please change this to something like `git://...`").

-define(OLD_CONFIG_FORMAT,
        "The current Elvis configuration file has an outdated format. "
        "Please check Elvis's GitHub repository to find out what the "
        "new format is.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec git_for_deps_erlang_mk(elvis_config:config(),
                               elvis_file:file(),
                               [term()]) ->
    [elvis_result:item()].
git_for_deps_erlang_mk(_Config, Target, RuleConfig) ->
    IgnoreDeps = maps:get(ignore, RuleConfig, []),
    Deps = get_erlang_mk_deps(Target),
    BadDeps = lists:filter(fun is_erlang_mk_not_git_dep/1, Deps),
    lists:flatmap(
        fun(Line) -> erlang_mk_dep_to_result(Line, IgnoreDeps) end, BadDeps).

-spec git_for_deps_rebar(elvis_config:config(),
                           elvis_file:file(),
                           [term()]) ->
    [elvis_result:item()].
git_for_deps_rebar(_Config, Target, RuleConfig) ->
    IgnoreDeps = maps:get(ignore, RuleConfig, []),
    Deps = get_rebar_deps(Target),
    BadDeps = lists:filter(fun is_rebar_not_git_dep/1, Deps),
    lists:flatmap(
        fun(Line) -> rebar_dep_to_result(Line, IgnoreDeps) end, BadDeps).

-spec no_deps_master_erlang_mk(elvis_config:config(),
                               elvis_file:file(),
                               [term()]) ->
    [elvis_result:item()].
no_deps_master_erlang_mk(_Config, Target, RuleConfig) ->
    IgnoreDeps = maps:get(ignore, RuleConfig, []),
    Deps = get_erlang_mk_deps(Target),
    BadDeps = lists:filter(fun is_erlang_mk_master_dep/1, Deps),
    lists:flatmap(
        fun(Line) -> erlang_mk_dep_to_result(Line, IgnoreDeps) end, BadDeps).

-spec no_deps_master_rebar(elvis_config:config(),
                           elvis_file:file(),
                           [term()]) ->
    [elvis_result:item()].
no_deps_master_rebar(_Config, Target, RuleConfig) ->
    IgnoreDeps = maps:get(ignore, RuleConfig, []),
    Deps = get_rebar_deps(Target),
    BadDeps = lists:filter(fun is_rebar_master_dep/1, Deps),
    lists:flatmap(
        fun(Line) -> rebar_dep_to_result(Line, IgnoreDeps) end, BadDeps).

-spec old_configuration_format(elvis_config:config(),
                               elvis_file:file(),
                               [term()]) ->
    [elvis_result:item()].
old_configuration_format(_Config, Target, _RuleConfig) ->
    Path = elvis_file:path(Target),
    {ok, [AllConfig]} = file:consult(Path),
    case proplists:get_value(elvis, AllConfig) of
        undefined -> [];
        ElvisConfig ->
            case is_old_config(ElvisConfig) of
                false -> [];
                true ->
                    [elvis_result:new(item, ?OLD_CONFIG_FORMAT, [])]
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

get_rebar_deps(File) ->
    Path = elvis_file:path(File),
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

is_rebar_not_git_dep({_AppName, _Vsn, {_SCM, "git://" ++ _, _Branch}}) -> false;
is_rebar_not_git_dep(_) -> true.

rebar_dep_to_result({AppName, _, _}, IgnoreDeps) ->
    case lists:member(AppName, IgnoreDeps) of
        true ->
            [];
        false ->
            [elvis_result:new(item, ?DEP_MASTER, [AppName])]
    end.


%%% erlang.mk

is_erlang_mk_master_dep(Line) ->
    case re:run(Line, "master *$", []) of
        nomatch -> false;
        _ -> true
    end.

is_erlang_mk_not_git_dep(Line) ->
    lager:alert("~p", [Line]), false.

get_erlang_mk_deps(File) ->
    {Src, _} = elvis_file:src(File),
    Lines = binary:split(Src, <<"\n">>, [global]),
    IsDepsLine = fun(Line) -> re:run(Line, "dep_", []) /= nomatch end,
    lists:filter(IsDepsLine, Lines).

erlang_mk_dep_to_result(Line, IgnoreDeps) ->
    Opts = [{capture, all_but_first, binary}],
    {match, [Name]} = re:run(Line, "dep_([^ ]*)", Opts),
    NameAtom = binary_to_atom(Name, utf8),
    case lists:member(NameAtom, IgnoreDeps) of
        true ->
            [];
        false ->
            [elvis_result:new(item, ?DEP_MASTER, [Name])]
    end.

%% Old config

is_old_config(ElvisConfig) ->
    case proplists:get_value(config, ElvisConfig) of
        undefined -> false;
        Config when is_map(Config) -> true;
        Config when is_list(Config) ->
            SrcDirsIsKey = fun(RuleGroup) ->
                                   maps:is_key(src_dirs, RuleGroup) orelse
                                       exists_old_rule(RuleGroup)
                           end,
            lists:filter(SrcDirsIsKey, Config) /= []
    end.

exists_old_rule(#{rules := Rules}) ->
    Filter = fun
                 ({_, _, Args}) when is_list(Args) ->
                     true;
                 (_) ->
                     false
             end,
    lists:filter(Filter, Rules) /= [].
