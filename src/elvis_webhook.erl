-module(elvis_webhook).

-export([event/1]).

-export_type([request/0]).

-type event() :: pull_request.
-type request() :: #{headers => map(), body => map()}.

-type github_info() ::
        {
          elvis_github:credentials(),
          elvis_github:repository(),
          integer(),
          [map()]
        }.

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
      #{<<"number">> := PR, <<"repository">> := Repository}) ->
    Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
    Cred = github_credentials(),

    {ok, GithubFiles} = elvis_github:pull_req_files(Cred, Repo, PR),
    ErlFiles = [file_info(Cred, Repo, F)
                || F = #{<<"filename">> := Path} <- GithubFiles,
                   elvis_utils:is_erlang_file(Path)],

    Config1 = Config#{files => ErlFiles},
    case elvis:rock(Config1) of
        {fail, Results} ->
            {ok, Comments} = elvis_github:pull_req_comments(Cred, Repo, PR),
            % io:format("Comments ~p~n", [Comments]),
            GithubInfo = {Cred, Repo, PR, Comments},
            comment_files(GithubInfo, Results),
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

%% @doc Gets a raw_url for a file and extracts the commit id from it.
-spec commit_id_from_raw_url(string(), string()) -> string().
commit_id_from_raw_url(Url, Filename) ->
    UrlString = elvis_utils:to_str(Url),
    Regex = ".+/raw/(.+)/" ++ Filename,
    {match, [_, {Pos, Len} | _]} = re:run(UrlString, Regex),
    string:substr(UrlString, Pos + 1, Len).

%% @doc Comment files that failed rules.
comment_files(GithubInfo, Results) ->
    Fun = fun(Result) ->
                  File = elvis_result:get_file(Result),
                  Rules = elvis_result:get_rules(Result),
                  comment_rules(GithubInfo, Rules, File)
          end,
    lists:foreach(Fun, Results).

comment_rules(GithubInfo, Rules, File) ->
    Fun = fun(Rule) ->
              Items = elvis_result:get_items(Rule),
              comment_lines(GithubInfo, Items, File)
          end,
    lists:foreach(Fun, Rules).

-spec comment_lines(github_info(), elvis_result:item(), elvis_utils:file()) ->
    ok.
comment_lines(_GithubInfo, [], _File) ->
    ok;
comment_lines(GithubInfo, [Item | Items], File) ->
    {Cred, Repo, PR, Comments} = GithubInfo,
    #{path := Path, commit_id := CommitId} = File,
    Message = elvis_result:get_message(Item),
    Info = elvis_result:get_info(Item),
    LineNum = elvis_result:get_line_num(Item),

    Text = list_to_binary(io_lib:format(Message, Info)),

    try
        comment_exists(Comments, Path, LineNum, Text),
        {ok, _Response} =
            elvis_github:pull_req_comment_line(Cred, Repo, PR, CommitId,
                                               Path, LineNum, Text)
    catch
        error:{badmatch, _} -> ok
    end,
    comment_lines(GithubInfo, Items, File).

comment_exists([], _Path, _Line, _Body) ->
    ok;
comment_exists([Comment | Comments], Path, Line, Body) ->
    #{<<"path">> := Path,
      <<"position">> := Line,
      <<"body">> := Body} = Comment,

    comment_exists(Comments, Path, Line, Body).
