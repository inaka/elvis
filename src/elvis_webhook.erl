-module(elvis_webhook).

-export([event/1]).

-export_type([request/0]).

-type event() :: pull_request.
-type request() :: #{ headers => map(), body => map()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec event(request()) -> ok | {error, term()}.
event(#{headers := Headers, body := Body}) ->
    EventName = maps:get(<<"X-GitHub-Event">>, Headers),
    EventData = jiffy:decode(Body, [return_maps]),
    Config = elvis_config:default(),

    event(Config, EventName, EventData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

-spec event(elvis_config:config(), event(), map()) -> ok | {error, term()}.
event(Config,
      <<"pull_request">>,
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
            comment_files(Cred, RepoName, PR, Results),
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
-spec github_credentials() -> elvis_github:credentials().
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

%% @doc Comment files that failed rules.
comment_files(Cred, Repo, PR, Results) ->
    Fun = fun(#{file := File, rules := Rules}) ->
              comment_rules(Cred, Repo, PR, Rules, File)
          end,
    lists:foreach(Fun, Results).

comment_rules(Cred, Repo, PR, Rules, File) ->
    Fun = fun(#{items := Items}) ->
              comment_lines(Cred, Repo, PR, Items, File)
          end,
    lists:foreach(Fun, Rules).

-spec comment_lines(elvis_github:credentials(),
                    elvis_github:repository(),
                    integer(),
                    elvis_result:file_item(),
                    {string(), atom()}) -> ok.
comment_lines(_Cred, _Repo, _PR, [], _File) ->
    ok;
comment_lines(Cred, Repo, PR,
              [#{line_num := Line,
                 message := Message,
                 info := Info}
               | Items],
              File = #{path := Path, commit_id := CommitId}) ->
    Comment = io_lib:format(Message, Info),
    {ok, _Response} =
        elvis_github:pull_req_comment_line(Cred, Repo, PR, CommitId,
                                           Path, Line, Comment),
    comment_lines(Cred, Repo, PR, Items, File).
