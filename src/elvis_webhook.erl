-module(elvis_webhook).

-behaviour(egithub_webhook).

-export([event/1, event/2]).
-export([handle_pull_request/3]).
-export([handle_error/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External Functions

-spec event(egithub_webhook_req:request()) -> ok | {error, term()}.
event(Request) ->
    event(github_credentials(), Request).

-spec event(egithub:credentials(), egithub_webhook_req:request()) -> ok | {error, term()}.
event(Cred, Request) ->
    egithub_webhook:event(?MODULE, Cred, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks

-spec handle_pull_request(egithub:credentials(),
                          egithub_webhook:req_data(),
                          [egithub_webhook:file()]) ->
                             {ok, egithub_webhook:review()}.
handle_pull_request(Cred, Data, GithubFiles) ->
    #{<<"repository">> := Repository} = Data,
    BranchName = maps_get([<<"pull_request">>, <<"head">>, <<"ref">>], Data, <<"master">>),
    Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
    Branch = binary_to_list(BranchName),
    Config = repo_config(Cred, Repo, Branch, elvis:default_config()),

    GithubFiles1 = [F#{path => Path} || F = #{<<"filename">> := Path} <- GithubFiles],
    Config1 = elvis_config:resolve_files(Config, GithubFiles1),

    FileInfoFun = fun(File) -> file_info(Cred, Repo, File) end,
    Config2 = elvis_config:apply_to_files(FileInfoFun, Config1),
    CommitId = maps_get([<<"pull_request">>, <<"head">>, <<"sha">>], Data, <<>>),

    case elvis_core:rock(Config2) of
        {fail, Results} ->
            {ok, review_from_results(Results, CommitId, GithubFiles1)};
        ok ->
            {ok,
             #{commit_id => CommitId,
               body => <<":+1:">>,
               event => <<"APPROVE">>,
               comments => []}}
    end.

-spec handle_error({error, term()},
                   egithub_webhook:req_data(),
                   [egithub_webhook:file()]) ->
                      {ok, [], []}.
handle_error({error, _}, _, _) ->
    {ok, [], []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

-spec review_from_results([elvis_result:file()], string(), [map()]) ->
                             egithub_webhook:review().
review_from_results(Results, CommitId, GithubFiles) ->
    RC = review_comments_from_results(Results, GithubFiles),
    %% Comments with `position = 0' cannot be added as a review comment, so they
    %% are added to the review's body.
    Fun = fun (#{position := 0, body := B}, {BodyAcc, CommentsAcc}) ->
                  {<<BodyAcc/binary, "\n- ", B/binary>>, CommentsAcc};
              (Comment, {BodyAcc, CommentsAcc}) ->
                  {BodyAcc, [Comment | CommentsAcc]}
          end,
    {Body, ReviewComments} = lists:foldl(Fun, {<<>>, []}, RC),
    #{commit_id => CommitId,
      body => Body,
      event => <<"REQUEST_CHANGES">>,
      comments => ReviewComments}.

-spec github_credentials() -> egithub:credentials().
github_credentials() ->
    User = application:get_env(elvis, github_user, ""),
    Password = application:get_env(elvis, github_password, ""),
    egithub:basic_auth(User, Password).

file_info(Cred,
          Repo,
          #{<<"filename">> := Filename,
            <<"raw_url">> := RawUrl,
            <<"patch">> := Patch}) ->
    CommitId = commit_id_from_raw_url(RawUrl, Filename),
    {ok, Content} = egithub:file_content(Cred, Repo, CommitId, Filename),
    #{path => Filename,
      content => Content,
      commit_id => CommitId,
      patch => Patch}.

repo_config(Cred, Repo, Branch, LocalConfig) ->
    case egithub:file_content(Cred, Repo, Branch, "elvis.config") of
        {ok, ConfigContent} ->
            ConfigEval = ktn_code:eval(ConfigContent),
            ElvisConfig = proplists:get_value(elvis, ConfigEval),
            proplists:get_value(config, ElvisConfig);
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

review_comments_from_results(Results, GithubFiles) ->
    GithubFiles1 = maps:from_list([{Path, File} || #{path := Path} = File <- GithubFiles]),
    lists:flatmap(fun(Result) -> review_comments_from_result(Result, GithubFiles1) end,
                  Results).

review_comments_from_result(Result, GithubFiles) ->
    FilePath = elvis_result:get_path(Result),
    #{patch := Patch} = maps:get(FilePath, GithubFiles),

    Rules = elvis_result:get_rules(Result),
    lists:flatmap(fun(Rule) -> review_comments_from_result(Rule, FilePath, Patch) end, Rules).

review_comments_from_result(Rule, FilePath, Patch) ->
    Items = elvis_result:get_items(Rule),
    lists:flatmap(fun(Item) -> review_comments_from_item(Item, FilePath, Patch) end, Items).

review_comments_from_item(Item, Path, Patch) ->
    Message = elvis_result:get_message(Item),
    Info = elvis_result:get_info(Item),
    Line = elvis_result:get_line_num(Item),
    Text = list_to_binary(io_lib:format(Message, Info)),

    case Line of
        0 ->
            [#{path => Path,
               position => Line,
               body => Text}];
        _ ->
            case elvis_git:relative_position(Patch, Line) of
                {ok, Position} ->
                    [#{path => Path,
                       position => Position,
                       body => Text}];
                not_found ->
                    Args = [Line],
                    ok = error_logger:info_msg("Line ~p does not belong to file's diff.", Args),
                    []
            end
    end.

maps_get([Key], Map, Default) ->
    maps:get(Key, Map, Default);
maps_get([Key | Rest], Map, Default) ->
    case maps:get(Key, Map, Default) of
        NewMap when is_map(NewMap) ->
            maps_get(Rest, NewMap, Default);
        _ ->
            Default
    end.
