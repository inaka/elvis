-module(elvis_webhook).

-export([event/2]).

-export_type([request/0]).

-type event() :: pull_request.
-type request() :: #{headers => map(), body => map()}.

-type github_info() ::
        {
          egithub:credentials(),
          egithub:repository(),
          integer(),
          [map()]
        }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec event(egithub:credentials(), request()) -> ok | {error, term()}.
event(Cred, #{headers := Headers, body := Body}) ->
    HeaderName = <<"x-github-event">>,
    case maps:is_key(HeaderName, Headers) of
        false ->
            {error, missing_header};
        true ->
            EventName = maps:get(HeaderName, Headers),
            EventData = jiffy:decode(Body, [return_maps]),
            Config = elvis_config:default(),
            event(Config, Cred, EventName, EventData)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

-spec event(elvis_config:config(),
            egithub:credentials(),
            event(),
            map()) -> ok | {error, term()}.
event(LocalConfig,
      Cred,
      <<"pull_request">>,
      #{<<"number">> := PR, <<"repository">> := Repository}) ->
    Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
    Config = repo_config(Cred, Repo, LocalConfig),

    {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),

    GithubFiles1 = [F#{path => Path}
                    || F = #{<<"filename">> := Path} <- GithubFiles],

    Config1 = elvis_config:resolve_files(Config, GithubFiles1),

    FileInfoFun = fun (File) -> file_info(Cred, Repo, File) end,
    Config2 = elvis_config:apply_to_files(FileInfoFun, Config1),

    case elvis:rock(Config2) of
        {fail, Results} ->
            {ok, Comments} = egithub:pull_req_comments(Cred, Repo, PR),
            GithubInfo = {Cred, Repo, PR, Comments},
            comment_files(GithubInfo, Results);
        ok -> ok
    end;

event(_Config, _Cred, <<"ping">>, _Data) ->
    ok;

event(_Config, _Cred, Event, _Data) ->
    {error, io_lib:format("Nothing to do for event: ~p.~n", [Event])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

file_info(Cred, Repo,
          #{<<"filename">> := Filename,
            <<"raw_url">> := RawUrl,
            <<"patch">> := Patch}) ->
    CommitId = commit_id_from_raw_url(RawUrl, Filename),
    {ok, Content} = egithub:file_content(Cred, Repo, CommitId, Filename),
    #{path => Filename,
      content => Content,
      commit_id => CommitId,
      patch => Patch}.

repo_config(Cred, Repo, LocalConfig) ->
    case egithub:file_content(Cred, Repo, "master", "elvis.config") of
        {ok, ConfigContent} ->
            ConfigEval = elvis_code:eval(ConfigContent),
            RepoConfig = elvis_config:load(ConfigEval),
            RepoConfig;
        {error, _} ->
            LocalConfig
    end.

%% @doc Gets a raw_url for a file and extracts the commit id from it.
-spec commit_id_from_raw_url(string(), string()) -> string().
commit_id_from_raw_url(Url, Filename) ->
    UrlString = elvis_utils:to_str(Url),
    Regex = ".+/raw/(.+)/" ++ Filename,
    {match, [_, {Pos, Len} | _]} = re:run(UrlString, Regex),
    string:substr(UrlString, Pos + 1, Len).

%% @doc Comment files that failed rules.
-spec comment_files(github_info(), [elvis_result:file()]) -> ok.
comment_files(GithubInfo, Results) ->
    Fun = fun(Result) ->
                  File = elvis_result:get_file(Result),
                  Rules = elvis_result:get_rules(Result),
                  comment_rules(GithubInfo, Rules, File)
          end,
    lists:foreach(Fun, Results).

-spec comment_rules(github_info(), [elvis_result:rule()], elvis_file:file()) ->
    ok.
comment_rules(GithubInfo, Rules, File) ->
    Fun = fun(Rule) ->
              Items = elvis_result:get_items(Rule),
              comment_lines(GithubInfo, Items, File)
          end,
    lists:foreach(Fun, Rules).

-spec comment_lines(github_info(), [elvis_result:item()], elvis_file:file()) ->
    ok.
comment_lines(_GithubInfo, [], _File) ->
    ok;
comment_lines(GithubInfo, [Item | Items], File) ->
    {Cred, Repo, PR, Comments} = GithubInfo,
    #{path := Path,
      commit_id := CommitId,
      patch := Patch} = File,
    Message = elvis_result:get_message(Item),
    Info = elvis_result:get_info(Item),
    Line = elvis_result:get_line_num(Item),

    case elvis_git:relative_position(Patch, Line) of
        {ok, Position} ->
            Text = list_to_binary(io_lib:format(Message, Info)),

            case comment_exists(Comments, Path, Position, Text) of
                exists ->
                    Args = [Text, Path, Line],
                    lager:info("Comment '~p' for ~p on line ~p exists", Args);
                not_exists ->
                    {ok, _} =
                        egithub:pull_req_comment_line(
                          Cred, Repo, PR, CommitId, Path, Position, Text
                        )
            end;
        not_found ->
            Args = [Line],
            lager:info("Line ~p does not belong to file's diff.", Args)
    end,

    comment_lines(GithubInfo, Items, File).

comment_exists([], _Path, _Line, _Body) ->
    not_exists;
comment_exists([Comment | Comments], Path, Position, Body) ->
    try
        #{<<"path">> := Path,
          <<"position">> := Position,
          <<"body">> := Body} = Comment,
        exists
    catch
        error:{badmatch, _} ->
            comment_exists(Comments, Path, Position, Body)
    end.
