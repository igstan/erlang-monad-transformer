%% Identity transformer used for exploring Erlang's AST.
-module(id_transform).

-export([parse_transform/2]).

parse_transform(Ast, Options) ->
  io:format("OPTIONS =============\n"),
  io:format("~p~n~n", [Options]),
  io:format("AST =================\n"),
  io:format("~p~n~n", [Ast]),
  Ast.
