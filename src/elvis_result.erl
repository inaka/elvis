-module(elvis_result).

-export([
         new/0,
         new/2,
         print/1
        ]).

-record(result,
        {
          message = "" :: string(),
          info = [] :: list()
        }).

-type result() :: #result{}.

-spec new() -> result().
new() ->
    #result{}.

-spec new(string(), list()) -> result().
new(Msg, Info) ->
    #result{message=Msg, info= Info}.

-spec print(result() | [result()]) -> ok.
print([]) ->
    ok;
print([Result | Results]) ->
    print(Result),
    print(Results);
print(#result{message = Msg, info = Info}) ->
    io:format("~p~n  ~p~n", [Msg, Info]),
    ok.
