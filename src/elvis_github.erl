%%% @doc Github API
-module(elvis_github).

%% Pull Requests
-export([
         basic_auth_credentials/0,
         %% Pull Requests
         pull_req_files/3,
         pull_req_comment_line/7,
         pull_req_comments/3,
         %% Users
         user/1,
         user/2,
         repo/2,
         repos/2,
         repos/3,
         all_repos/2,
         all_repos/3,
         orgs/1,
         orgs/2,
         org_repos/3,
         all_org_repos/3,
         %% Teams
         teams/2,
         create_team/5,
         add_team_member/3,
         delete_team_member/3,
         %% Hooks
         hooks/2,
         create_webhook/4,
         delete_webhook/3,
         %% Collaborators
         collaborators/2,
         add_collaborator/3,
         remove_collaborator/3
        ]).

%% Files
-export([
         file_content/4
        ]).

-export_type([
              credentials/0,
              repository/0
             ]).

-type credentials() ::
    {basic, Username :: string(), Password :: string()}
    | {oauth, Token :: string()}.
-type repository() :: string(). %% "username/reponame"
-type result() :: ok | {ok, any()} | {error, term()}.

-define(GITHUB_API, "https://api.github.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Gets the github basic auth credentials form the elvis app environment.
-spec basic_auth_credentials() -> credentials().
basic_auth_credentials() ->
    User = application:get_env(elvis, github_user, ""),
    Password = application:get_env(elvis, github_password, ""),
    {basic, User, Password}.

%% Pull Requests

-spec pull_req_files(credentials(), repository(), integer()) ->
    result().
pull_req_files(Credentials, Repo, PR) ->
    Url = make_url({pull_req, files}, {Repo, PR}),
    {ok, Result} = auth_req(Credentials, Url),
    Files = jiffy:decode(Result, [return_maps]),
    {ok, Files}.

-spec pull_req_comment_line(credentials(), repository(), integer(),
                            string(), string(), integer(), string()) ->
    result().
pull_req_comment_line(Credentials, Repo, PR,
                      CommitId, Filename, Line, Text) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    Body = #{<<"commit_id">> => list_to_binary(CommitId),
             <<"path">> => Filename,
             <<"position">> => Line,
             <<"body">> => Text
            },
    JsonBody = jiffy:encode(Body),
    auth_req(Credentials, Url, post, JsonBody).

-spec pull_req_comments(credentials(), repository(), integer()) ->
    result().
pull_req_comments(Cred, Repo, PR) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    {ok, Result} = auth_req(Cred, Url),
    Comments = jiffy:decode(Result, [return_maps]),
    {ok, Comments}.

%% Files

-spec file_content(credentials(), repository(), string(), string()) -> result().
file_content(Cred, Repo, CommitId, Filename) ->
    Url = make_url(file_content, {Repo, CommitId, Filename}),
    case auth_req(Cred, Url) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            ContentBase64 = maps:get(<<"content">>, JsonResult),
            Content = base64:decode(ContentBase64),
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

%% Users

-spec user(credentials()) -> result().
user(Cred) ->
    Url = make_url(user, {}),
    api_call_json_result(Cred, Url).

-spec user(credentials(), string()) -> result().
user(Cred, Username) ->
    Url = make_url(user, {Username}),
    api_call_json_result(Cred, Url).

%% Orgs

-spec orgs(credentials()) -> string().
orgs(Cred) ->
    orgs(Cred, undefined).

-spec orgs(credentials(), string()) -> string().
orgs(Cred, User) ->
    Url = make_url(orgs, {User}),
    api_call_json_result(Cred, Url).

%% Repos

-spec repo(credentials(), string()) -> result().
repo(Cred, RepoFullName) ->
    Url = make_url(repo, {RepoFullName}),
    api_call_json_result(Cred, Url).

-spec repos(credentials(), map()) -> result().
repos(Cred, Opts) ->
    repos(Cred, undefined, Opts).

-spec repos(credentials(), string(), map()) -> result().
repos(Cred, User, Opts) ->
    Url = make_url(repos, {User, Opts}),
    api_call_json_result(Cred, Url).

-spec all_repos(credentials(), map()) -> result().
all_repos(Cred, Opts) ->
    all_repos(Cred, undefined, Opts#{page => 1}, []).

-spec all_repos(credentials(), string(), map()) -> result().
all_repos(Cred, User, Opts) ->
    all_repos(Cred, User, Opts#{page => 1}, []).

all_repos(Cred, User, Opts = #{page := Page}, Results) ->
    case repos(Cred, User, Opts) of
        {ok, []} ->
            {ok, lists:flatten(Results)};
        {ok, Repos} ->
            all_repos(Cred, User, Opts#{page => Page + 1}, [Repos | Results]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec org_repos(credentials(), string(), map()) -> result().
org_repos(Cred, Org, Opts) ->
    Url = make_url(org_repos, {Org, Opts}),
    api_call_json_result(Cred, Url).

-spec all_org_repos(credentials(), string(), map()) -> result().
all_org_repos(Cred, Org, Opts) ->
    all_org_repos(Cred, Org, Opts#{page => 1}, []).

all_org_repos(Cred, Org, Opts = #{page := Page}, Results) ->
    case org_repos(Cred, Org, Opts) of
        {ok, []} ->
            {ok, lists:flatten(Results)};
        {ok, Repos} ->
            NewResults = [Repos | Results],
            NewOpts = Opts#{page => Page + 1},
            all_org_repos(Cred, Org, NewOpts, NewResults);
        {error, Reason} ->
            {error, Reason}
    end.

%% Teams

-spec teams(credentials(), string()) -> result().
teams(Cred, Org) ->
    Url = make_url(teams, {Org}),
    api_call_json_result(Cred, Url).

-spec create_team(credentials(), string(), string(), string(), [string()]) ->
    result().
create_team(Cred, Org, Name, Permission, Repos) ->
    Url = make_url(teams, {Org}),
    BodyMap = #{name => list_to_binary(Name),
                 permission => list_to_binary(Permission),
                 repo_names => list_to_binary(Repos)},
    Body = jiffy:encode(BodyMap),
    case auth_req(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            {ok, JsonResult};
        {error, {"422", _, _}} ->
            {ok, already_exists};
        Other ->
            Other
    end.

-spec add_team_member(credentials(), integer(), string()) -> result().
add_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    Body = [],
    case auth_req(Cred, Url, put, Body) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

-spec delete_team_member(credentials(), integer(), string()) -> result().
delete_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    Body = [],
    case auth_req(Cred, Url, delete, Body) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

%% Hooks

-spec hooks(credentials(), repository()) -> result().
hooks(Cred, Repo) ->
    Url = make_url(hooks, {Repo}),
    api_call_json_result(Cred, Url).

-spec create_webhook(credentials(), repository(), string(), [string()]) ->
    result().
create_webhook(Cred, Repo, WebhookUrl, Events) ->
    Url = make_url(hooks, {Repo}),
    BinEvents = [list_to_binary(E) || E <- Events],
    Data = #{<<"name">> => <<"web">>,
             <<"active">> => true,
             <<"events">> => BinEvents,
             <<"config">> => #{<<"url">> => list_to_binary(WebhookUrl),
                               <<"content_type">> => <<"json">>}},
    Body = jiffy:encode(Data),
    case auth_req(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_webhook(credentials(), repository(), string()) -> result().
delete_webhook(Cred, Repo, Id) ->
    IdStr = elvis_utils:to_str(Id),
    Url = make_url(hooks, {Repo, IdStr}),
    Body = [],
    case auth_req(Cred, Url, delete, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Collaborators

-spec collaborators(credentials(), repository()) -> result().
collaborators(Cred, Repo) ->
    Url = make_url(collaborators, {Repo}),
    case auth_req(Cred, Url) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

-spec add_collaborator(credentials(), repository(), string()) -> result().
add_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    Body = [],
    case auth_req(Cred, Url, put, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove_collaborator(credentials(), repository(), string()) -> result().
remove_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    Body = [],
    case auth_req(Cred, Url, delete, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Pull Resquest
make_url({pull_req, Subentity}, {Repo, PR}) ->
    SubentityStr = elvis_utils:to_str(Subentity),
    Url = ?GITHUB_API ++ "/repos/~s/pulls/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);

%% Files
make_url(file_content, {Repo, CommitId, Filename}) ->
    Url = ?GITHUB_API ++ "/repos/~s/contents/~s?ref=~s",
    io_lib:format(Url, [Repo, Filename, CommitId]);

%% User
make_url(user, {}) ->
    Url = ?GITHUB_API ++ "/user",
    io_lib:format(Url, []);
make_url(user, {Username}) ->
    Url = ?GITHUB_API ++ "/users/~s",
    io_lib:format(Url, [Username]);

%% Organizations
make_url(orgs, {undefined}) ->
    Url = ?GITHUB_API ++ "/user/orgs",
    io_lib:format(Url, []);
make_url(orgs, {User}) ->
    Url = ?GITHUB_API ++ "/users/~s/orgs",
    io_lib:format(Url, [User]);

%% Teams
make_url(teams, {Org}) ->
    Url = ?GITHUB_API ++ "/orgs/~s/teams",
    io_lib:format(Url, [Org]);
make_url(teams, {TeamId, Username}) ->
    Url = ?GITHUB_API ++ "/teams/~p/members/~s",
    io_lib:format(Url, [TeamId, Username]);

%% Repositories
make_url(repo, {RepoFullName}) ->
    Url = ?GITHUB_API ++ "/repos/~s",
    io_lib:format(Url, [RepoFullName]);
make_url(repos, {User, Opts}) ->
    Type = elvis_utils:maps_get(type, Opts, "all"),
    Sort = elvis_utils:maps_get(sort, Opts, "full_name"),
    Direction = elvis_utils:maps_get(direction, Opts, "asc"),
    Page = elvis_utils:maps_get(page, Opts, 1),
    case User of
        undefined ->
            Url = ?GITHUB_API
                ++ "/user/repos?type=~s&sort=~s&direction=~s&page=~p",
            io_lib:format(Url, [Type, Sort, Direction, Page]);
        User ->
            Url = ?GITHUB_API ++ "/users/~s/repos?page=~p",
            io_lib:format(Url, [User, Page])
    end;
make_url(org_repos, {User, Opts}) ->
    Page = elvis_utils:maps_get(page, Opts, 1),
    Url = ?GITHUB_API ++ "/orgs/~s/repos?page=~p",
    io_lib:format(Url, [User, Page]);

%% Hooks
make_url(hooks, {Repo}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks",
    io_lib:format(Url, [Repo]);
make_url(hooks, {Repo, Id}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks/~s",
    io_lib:format(Url, [Repo, Id]);

%% Colaborators
make_url(collaborators, {Repo}) ->
    Url = ?GITHUB_API ++ "/repos/~s/collaborators",
    io_lib:format(Url, [Repo]);
make_url(collaborators, {Repo, Username}) ->
    Url = ?GITHUB_API ++ "/repos/~s/collaborators/~s",
    io_lib:format(Url, [Repo, Username]).

-spec auth_req(credentials(), string()) -> string() | {error, term()}.
auth_req(Credentials, Url) ->
    auth_req(Credentials, Url, get, []).

-spec auth_req(credentials(), string(), ibrowse:method(), ibrowse:body()) ->
    {ok, string()} | {error, term()}.
auth_req(Cred, Url, Method, Body) ->
    Options0 = [{ssl_options, [{depth, 0}]}],
    Headers0 = [{"User-Agent", "Elvis-Webhook"}],
    {Options, Headers} = authorization(Cred, Options0, Headers0),
    lager:info("[Github API] ~s", [Url]),
    case ibrowse:send_req(Url, Headers, Method, Body, Options) of
        {ok, "200", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "201", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "204", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "302", RespHeaders, _} ->
            RedirectUrl = proplists:get_value("Location", RespHeaders),
            auth_req(Cred, RedirectUrl, Method, Body);
        {ok, Status, RespHeaders, RespBody} ->
            {error, {Status, RespHeaders, RespBody}}
        end.

authorization({basic, Username, Password}, Options0, Headers) ->
    Options = [{basic_auth, {Username, Password}} | Options0],
    {Options, Headers};
authorization({oauth, Token}, Options, Headers0) ->
    Headers = [{"Authorization", "token " ++ Token} | Headers0],
    {Options, Headers}.

api_call_json_result(Cred, Url) ->
    case auth_req(Cred, Url) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.
