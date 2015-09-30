-module(elvis_file).

-export([
         src/1,
         path/1,
         parse_tree/2,
         load_file_data/2,

         find_files/2,
         find_files/3,
         filter_files/3
        ]).

-export_type([file/0]).

-type file() :: #{path => string(), content => binary()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a tuple with the contents of the file and the file itself.
-spec src(file()) ->
    {binary(), file()} | {error, enoent}.
src(File = #{content := Content}) ->
    {Content, File};
src(File = #{path := Path}) ->
    case file:read_file(Path) of
        {ok, Content} ->
            src(File#{content => Content});
        Error -> Error
    end;
src(File) ->
    throw({invalid_file, File}).

%% @doc Given a file() returns its path.
-spec path(file()) -> string().
path(#{path := Path}) ->
    Path;
path(File) ->
    throw({invalid_file, File}).

%% @doc Add the root node of the parse tree to the file data.
-spec parse_tree(elvis_config:config(), file()) ->
    {ktn_code:tree_node(), file()}.
parse_tree(_Config, File = #{parse_tree := ParseTree}) ->
    {ParseTree, File};
parse_tree(Config, File = #{path := Path, content := Content}) ->
    Ext = filename:extension(Path),
    ExtStr = elvis_utils:to_str(Ext),
    ParseTree = resolve_parse_tree(Config, ExtStr, Content),
    parse_tree(Config, File#{parse_tree => ParseTree});
parse_tree(Config, File0 = #{path := _Path}) ->
    {_, File} = src(File0),
    parse_tree(Config, File);
parse_tree(_Config, File) ->
    throw({invalid_file, File}).

%% @doc Loads and adds all related file data.
-spec load_file_data(elvis_config:config(), file()) -> file().
load_file_data(Config, File0 = #{path := _Path}) ->
    {_, File1} = src(File0),
    {_, File2} = parse_tree(Config, File1),
    File2.

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [file()].
find_files(Dirs, Pattern) ->
    find_files(Dirs, Pattern, recursive).

-spec find_files([string()], string(), recursive | local) -> [file()].
find_files(Dirs, Pattern, Option) ->
    MiddlePath = case Option of
                     recursive -> "/**/";
                     local -> "/"
                 end,
    Fun = fun(Dir) ->
              filelib:wildcard(Dir ++ MiddlePath ++ Pattern)
          end,
    [#{path => Path} || Path <- lists:flatmap(Fun, Dirs)].

%% @doc Filter files based on the glob provided.
-spec filter_files([file()], [string()], string()) -> [file()].
filter_files(Files, Dirs, Filter) ->
    FullFilters = lists:map(fun(Dir) -> Dir ++ "/" ++ Filter end, Dirs),
    Regexes = lists:map(fun glob_to_regex/1, FullFilters),
    FlatmapFun =
        fun(Regex) ->
                FilterFun =
                    fun(#{path := Path}) ->
                            match == re:run(Path, Regex, [{capture, none}])
                    end,
                lists:filter(FilterFun, Files)
        end,

    lists:flatmap(FlatmapFun, Regexes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec resolve_parse_tree(elvis_config:config(), string(), binary()) ->
    undefined | ktn_code:tree_node().
resolve_parse_tree(Config, ".erl", Content) ->
    ktn_code:parse_tree(Config, Content);
resolve_parse_tree(_, _, _) ->
    undefined.

-spec glob_to_regex(iodata()) -> iodata().
glob_to_regex(Glob) ->
    Regex1 = re:replace(Glob, "\\.", "\\\\.", [global]),
    re:replace(Regex1, "\\*", ".*", [global]).
