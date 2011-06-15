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
%%   bind(char($a), fun (A) ->
%%     bind(char($b), fun (B) ->
%%       return({A,B})
%%     end)
%%   end).
