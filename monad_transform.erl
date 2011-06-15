-module(monad_transform).

-export([parse_transform/2]).
-export([transform_node/3, transform_nodes/2, transform_function/2]).

parse_transform(Ast, _Options) ->
  {_,_, Nodes} = lists:foldl(fun transform_module/2, {false, nil, []}, Ast),
  Transformed = lists:reverse(Nodes),
  io:format("~s~n", [ pretty_print(Transformed) ]),
  Transformed.

pretty_print(Ast) ->
  lists:flatten([erl_pp:form(Node) || Node <- Ast]).

transform_module({attribute, _Line, monad, Module}, {false, _, Nodes}) ->
  {true, Module, Nodes};
transform_module(Node, {true, Module, Nodes}) ->
  {false, nil, [transform_function(Node, Module) | Nodes]};
transform_module(Node, {_, _, Nodes}) ->
  {false, nil, [Node | Nodes]}.

transform_function(Function, Module) ->
  case Function of
    {function, Line, Name, Arity, Clauses} ->
      {function, Line, Name, Arity,
        [transform_clause(Clause, Module) || Clause <- Clauses]}
  end.

transform_clause(Clause, Module) ->
  case Clause of
    {clause, Line, Args, Guards, Body} ->
      {clause, Line, Args, Guards, transform_nodes(Body, Module)}
  end.

transform_nodes(Expressions, Module) ->
  lists:foldr(fun (Expr, Continuation) ->
    transform_node(Expr, Continuation, Module)
  end, [], Expressions).

transform_node(Node, Continuation, Module) ->
  EmptyGuards = [],
  case Node of
    {call, Line, Return={atom,_,return}, Args} ->
      [{call, Line,
        {remote, Line, {atom, Line, Module}, Return},
        Args}];
    {match, Line, Pattern, Expression} ->
      [{call, Line,
        {remote, Line, {atom, Line, Module}, {atom, Line, bind}},
        [ Expression,
          {'fun', Line,
            {clauses,
              [{clause, Line,
                [Pattern],
                EmptyGuards,
                Continuation}]}}]}]
  end.
