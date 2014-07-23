%%% @doc Github API
-module(elvis_github).

%% Pull Requests
-export([
         pull_req_files/3,
         pull_req_comment_line/7,
         pull_req_comments/3
        ]).

%% Files
-export([
         file_content/4
        ]).

-export_type([
              credentials/0,
              repository/0
             ]).

-type credentials() :: {Username :: string(), Password :: string()}.
-type repository() :: string(). %% "username/reponame"
-type result() :: ok | {ok, any()} | {error, term()}.

-type endpoint() ::
    {pull_req, comments | files}
    | raw_contents.

-define(GITHUB_API, "https://api.github.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-spec file_content(credentials(), repository(), string(), string()) ->
    {ok, binary()}.
file_content(Cred, Repo, CommitId, Filename) ->
    Url = make_url(raw_contents, {Repo, CommitId, Filename}),
    case auth_req(Cred, Url) of
        {ok, Result} ->
            JsonResult = jiffy:decode(Result, [return_maps]),
            ContentBase64 = maps:get(<<"content">>, JsonResult),
            Content = base64:decode(ContentBase64),
            {ok, Content};
        {error, Reason} ->
            throw(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_url(endpoint(), any()) -> string().
make_url({pull_req, Subentity}, {Repo, PR}) ->
    SubentityStr = elvis_utils:to_str(Subentity),
    Url = ?GITHUB_API ++ "/repos/~s/pulls/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);
make_url(raw_contents, {Repo, CommitId, Filename}) ->
    Url = ?GITHUB_API ++ "/repos/~s/contents/~s?ref=~s",
    io_lib:format(Url, [Repo, Filename, CommitId]).

-spec auth_req(credentials(), string()) -> string() | {error, term()}.
auth_req(Credentials, Url) ->
    auth_req(Credentials, Url, get, []).

-spec auth_req(credentials(), string(), ibrowse:method(), ibrowse:body()) ->
    {ok, string()} | {error, term()}.
auth_req({Username, Password}, Url, Method, Body) ->
    Options = [{basic_auth, {Username, Password}},
               {ssl_options, [{depth, 0}]}],
    Headers = [{"User-Agent", "Elvis-Webhook"}],
    lager:info("[Github API] ~s", [Url]),
    case ibrowse:send_req(Url, Headers, Method, Body, Options) of
        {ok, "200", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "201", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "302", RespHeaders, _} ->
            RedirectUrl = proplists:get_value("Location", RespHeaders),
            auth_req({Username, Password}, RedirectUrl, Method, Body);
        {ok, Status, RespHeaders, RespBody} ->
            {error, {Status, RespHeaders, RespBody}}
        end.
