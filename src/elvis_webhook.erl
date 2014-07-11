-module(elvis_webhook).

-export([event/1]).

-type event() :: pull_request.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec event(map()) -> ok | {error, term()}.
event(#{headers := Headers, body := Body}) ->
    Event = maps:get(<<"X-GitHub-Event">>, Headers),
    EventAtom = list_to_atom(binary_to_list(Event)),
    Data = jiffy:decode(Body, [return_maps]),
    Config = elvis_config:default(),
    event(Config, EventAtom, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

-spec event(elvis_config:config(), event(), map()) -> ok | {error, term()}.
event(Config,
      pull_request,
      #{<<"number">> := PR, <<"repository">> := Repo}) ->
    RepoName = binary_to_list(maps:get(<<"full_name">>, Repo)),
    Cred = github_credentials(),

    {ok, GithubFiles} = elvis_github:pull_req_files(Cred, RepoName, PR),
    ErlFiles = [file_info(Cred, RepoName, F)
                || F = #{<<"filename">> := Path} <- GithubFiles,
                   elvis_utils:is_erlang_file(Path)],

    Config1 = Config#{files => ErlFiles},
    case elvis:rock(Config1) of
        {fail, Results} ->
            %% TODO: Comment each line that failed.
            {fail, Results};
        ok -> ok
    end;

event(_Config, Event, _Data) ->
    {error, io:format("Nothing to do for event: ~p.~n", [Event])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

file_info(Cred, Repo,
          #{<<"filename">> := Filename, <<"raw_url">> := RawUrl}) ->
    CommitId = commit_id_from_raw_url(RawUrl, Filename),
    {ok, Content} = elvis_github:file_content(Cred, Repo, CommitId, Filename),
    #{path => Filename, content => Content, commit_id => CommitId}.

%% @doc Gets the github
-spec github_credentials() -> [string()].
github_credentials() ->
    User = application:get_env(elvis, github_user, ""),
    Password = application:get_env(elvis, github_password, ""),
    {User, Password}.

%% @doc Get a raw_url for a file and extracts the commit id form it.
-spec commit_id_from_raw_url(string(), string()) -> string().
commit_id_from_raw_url(Url, Filename) ->
    UrlString = elvis_utils:to_str(Url),
    Regex = ".+/raw/(.+)/" ++ Filename,
    {match, [_, {Pos, Len} | _]} = re:run(UrlString, Regex),
    string:substr(UrlString, Pos + 1, Len).
