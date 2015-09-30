-module(fail_operator_spaces).

-export([function1/2,function2/2, function3/2, function4/2, function5/0, tag_filters/2]).

%% No space before and after coma,on a comment.

function1(Should,Fail) ->
    [Should,Fail].

function2(Shouldnt, Fail) ->
    _Unless = [we, consider]++ [operands, as, well],
    _WithDash = Shouldnt - Fail.

function3(Shouldnt, Fail) ->
    {
      Shouldnt ++ "Hello,Dont't Fail" ++ Fail,
      'hello,don\'t fail',
      <<"hello,don't fail">>
    }.

function4(Should, <<_:10/binary, ",", _/binary>>) ->
    Should = [$,, "where $, represents the character ,"].

function5() ->
    User = #{name => <<"Juan">>, email => <<"juan@inaka.com">>},
    <<"juan@inaka.com">> = maps:get(email,User).

tag_filters(DocName, #{conn := Conn} = State) ->
  TableName = atom_to_list(DocName),
  Sql = ["SELECT "
         " 'tag' AS \"type\", "
         " tag_name AS value, "
         " COUNT(1) AS \"count\" "
         "FROM ( "
         " SELECT unnest(regexp_split_to_array(tags, ',')) AS tag_name"
         " FROM ", TableName, " "
         ") AS tags "
         "GROUP BY tag_name "
         "ORDER BY tag_name "],
  Values = [],
  case {Conn, Sql, Values} of
    {ok, Maps, _} ->
      {ok, {raw, Maps}, State};
    {error, Error, _} ->
      {error, Error, State}
  end.
