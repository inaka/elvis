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

-define(GITHUB, "https://github.com").
-define(GITHUB_API, "https://api.github.com").

-define(PULL_REQS, ?GITHUB_API ++ "/repos/~s/pulls/~p").
-define(RAW_CONTENTS, ?GITHUB ++ "/~s/raw/~s/~s").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec pull_req_files(credentials(), repository(), integer()) ->
    result().
pull_req_files(Credentials, Repo, PR) ->
    URL = io_lib:format(?PULL_REQS ++ "/files", [Repo, PR]),
    {ok, Result} = auth_req(Credentials, URL),
    Files = jiffy:decode(Result, [return_maps]),
    {ok, Files}.

-spec pull_req_comment_line(credentials(), repository(), integer(),
                            string(), string(), integer(), string()) ->
    result().
pull_req_comment_line(Credentials, Repo, PR,
                      CommitId, Filename, Line, Text) ->
    URL = io_lib:format(?PULL_REQS ++ "/comments", [Repo, PR]),
    Body = #{<<"commit_id">> => list_to_binary(CommitId),
             <<"path">> => Filename,
             <<"position">> => Line,
             <<"body">> => Text
            },
    JsonBody = jiffy:encode(Body),
    auth_req(Credentials, URL, post, JsonBody).

-spec pull_req_comments(credentials(), repository(), integer()) ->
    result().
pull_req_comments(Cred, Repo, PR) ->
    URL =  io_lib:format(?PULL_REQS ++ "/comments", [Repo, PR]),
    {ok, Result} = auth_req(Cred, URL),
    Comments = jiffy:decode(Result, [return_maps]),
    {ok, Comments}.

-spec file_content(credentials(), repository(), string(), string()) ->
    binary().
file_content(Cred, Repo, CommitId, Filename) ->
    URL = io_lib:format(?RAW_CONTENTS, [Repo, CommitId, Filename]),
    {ok, Content} = auth_req(Cred, URL),
    {ok, list_to_binary(Content)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec auth_req(credentials(), string()) -> binary().
auth_req(Credentials, URL) ->
    auth_req(Credentials, URL, get, []).

-spec auth_req(credentials(), string(), ibrowse:method(), ibrowse:body()) ->
    {ok, binary()} | {error, term()}.
auth_req({Username, Password}, URL, Method, Body) ->
    Options = [{basic_auth, {Username, Password}},
               {ssl_options, [{depth, 0}]}],
    Headers = [{"User-Agent", "Elvis-Webhook"}],
    io:format("[Github API] ~s~n", [URL]),
    case ibrowse:send_req(URL, Headers, Method, Body, Options) of
        {ok, "200", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "201", _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, "302", RespHeaders, _} ->
            RedirectURL = proplists:get_value("Location", RespHeaders),
            auth_req({Username, Password}, RedirectURL, Method, Body);
        {ok, Status, RespHeaders, RespBody} ->
            {error, {Status, RespHeaders, RespBody}}
        end.
