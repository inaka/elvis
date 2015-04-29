-module(elvis_webhook).

-behaviour(egithub_webhook).

-export([event/2]).
-export([handle_pull_request/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External Functions

-spec event(egithub:credentials(), egithub_webhook:request()) ->
  ok | {error, term()}.
event(Cred, Request) -> egithub_webhook:event(?MODULE, Cred, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks

-spec handle_pull_request(
  egithub:credentials(), egithub_webhook:req_data(),
  [egithub_webhook:file()]) ->
  {ok, [egithub_webhook:message()]} | {error, term()}.
handle_pull_request(Cred, Data, GithubFiles) ->
    #{<<"repository">> := Repository} = Data,
    BranchName = ktn_maps:get([<<"pull_request">>,
                               <<"base">>,
                               <<"repo">>,
                               <<"default_branch">>],
                              Data, <<"master">>),
    Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
    Branch = binary_to_list(BranchName),
    Config = repo_config(Cred, Repo, Branch, elvis_config:default()),

    GithubFiles1 = [F#{path => Path}
                    || F = #{<<"filename">> := Path} <- GithubFiles],

    Config1 = elvis_config:resolve_files(Config, GithubFiles1),

    FileInfoFun = fun (File) -> file_info(Cred, Repo, File) end,
    Config2 = elvis_config:apply_to_files(FileInfoFun, Config1),

    case elvis:rock(Config2) of
        {fail, Results} -> {ok, messages_from_results(Results)};
        ok -> {ok, []}
    end.

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

repo_config(Cred, Repo, Branch, LocalConfig) ->
    case egithub:file_content(Cred, Repo, Branch, "elvis.config") of
        {ok, ConfigContent} ->
            ConfigEval = ktn_code:eval(ConfigContent),
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

messages_from_results(Results) ->
    lists:flatmap(
        fun(Result) ->
            messages_from_result(Result)
        end, Results).

messages_from_result(Result) ->
    File = elvis_result:get_file(Result),
    Rules = elvis_result:get_rules(Result),
    lists:flatmap(
        fun(Rule) ->
            messages_from_result(Rule, File)
        end, Rules).

messages_from_result(Rule, File) ->
    Items = elvis_result:get_items(Rule),
    lists:flatmap(
        fun(Item) ->
            messages_from_item(Item, File)
        end, Items).

messages_from_item(Item, File) ->
    #{path := Path,
      commit_id := CommitId,
      patch := Patch} = File,
    Message = elvis_result:get_message(Item),
    Info = elvis_result:get_info(Item),
    Line = elvis_result:get_line_num(Item),
    Text = list_to_binary(io_lib:format(Message, Info)),

    case Line of
        0 ->
            [#{text => Text,
               position => Line}];
        _ ->
            case elvis_git:relative_position(Patch, Line) of
                {ok, Position} ->
                    [ #{commit_id => CommitId,
                        path      => Path,
                        position  => Position,
                        text      => Text
                       }
                    ];
                not_found ->
                    Args = [Line],
                    lager:info("Line ~p does not belong to file's diff.", Args),
                    []
            end
    end.
