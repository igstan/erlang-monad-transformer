-module(monads).
-compile([{parse_transform, monad_transform}]).
-export([expr/0]).
-import(parser, [char/1]).

-monad(parser).
expr() ->
  A = char($a),
  B = char($b),
  return({A,B}).

%% The above should be transformed to this
%%
%% expr() ->
%%   parser:bind(char($a), fun (A) ->
%%     parser:bind(char($b), fun (B) ->
%%       parser:return({A,B})
%%     end)
%%   end).
