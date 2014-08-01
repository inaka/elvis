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
         current_user_repos/2,
         current_user_orgs/1,
         repos/2,
         org_repos/2,
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

-type endpoint() ::
    {pull_req, comments | files}
    | raw_contents.

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

-spec user(credentials()) -> result().
user(Cred) ->
    Url = make_url(user, {}),
    api_call_json_result(Cred, Url).

-spec current_user_repos(credentials(), map()) -> string().
current_user_repos(Cred, Opts) ->
    Url = make_url(user_repos, {Opts}),
    api_call_json_result(Cred, Url).

-spec current_user_orgs(credentials()) -> string().
current_user_orgs(Cred) ->
    Url = make_url(user_orgs, {}),
    api_call_json_result(Cred, Url).

-spec repos(credentials(), string()) -> result().
repos(Cred, User) ->
    Url = make_url(repos, {User}),
    api_call_json_result(Cred, Url).

-spec org_repos(credentials(), string()) -> result().
org_repos(Cred, Org) ->
    Url = make_url(org_repos, {Org}),
    api_call_json_result(Cred, Url).

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

-spec make_url(endpoint(), any()) -> string().
make_url({pull_req, Subentity}, {Repo, PR}) ->
    SubentityStr = elvis_utils:to_str(Subentity),
    Url = ?GITHUB_API ++ "/repos/~s/pulls/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);
make_url(file_content, {Repo, CommitId, Filename}) ->
    Url = ?GITHUB_API ++ "/repos/~s/contents/~s?ref=~s",
    io_lib:format(Url, [Repo, Filename, CommitId]);
make_url(user, {}) ->
    Url = ?GITHUB_API ++ "/user",
    io_lib:format(Url, []);
make_url(user_repos, {Opts}) ->
    Type = elvis_utils:maps_get(type, Opts, "all"),
    Sort = elvis_utils:maps_get(sort, Opts, "full_name"),
    Direction = elvis_utils:maps_get(direction, Opts, "asc"),
    Url = ?GITHUB_API ++ "/user/repos?type=~s&sort=~s&direction=~s",
    io_lib:format(Url, [Type, Sort, Direction]);
make_url(user_orgs, {}) ->
    Url = ?GITHUB_API ++ "/user/orgs",
    io_lib:format(Url, []);
make_url(repos, {User}) ->
    Url = ?GITHUB_API ++ "/users/~s/repos",
    io_lib:format(Url, [User]);
make_url(org_repos, {User}) ->
    Url = ?GITHUB_API ++ "/users/~s/repos",
    io_lib:format(Url, [User]);
make_url(hooks, {Repo}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks",
    io_lib:format(Url, [Repo]);
make_url(hooks, {Repo, Id}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks/~s",
    io_lib:format(Url, [Repo, Id]);
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
