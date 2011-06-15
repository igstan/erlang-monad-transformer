-module(parser).

-compile({parse_transform, monad_transform}).

-export([bind/2, return/1, zero/0, plus/2, item/0, satisfies/1, char/1]).
-export([many/1, many1/1, string/1, lower/0, upper/0, digit/0]).
-export([letter/0, letters/0, one_of/1, none_of/1, space/0, spaces/0]).
-export([alpha_num/0, sep_by/2, sep_by1/2, between/3, choice/1]).
-export([one_of_strings/1, seq/2, digits/0, chain_left1/2]).

-import(lists, [member/2, foldl/3, map/2, reverse/1]).

bind(Prev, Bridge) ->
  fun (Input) ->
    case Prev(Input) of
      {}            -> {};
      {Value,State} -> (Bridge(Value))(State)
    end
  end.

seq(Parsers, Bridge) ->
  fun (Input) ->
    try
      lists:foldl(fun (Parser, Acc) ->
        [{_, State} | _] = Acc,

        case Parser(State) of
          {}     -> throw(true);
          Result -> [Result | Acc]
        end
      end, [{nil,Input}], Parsers)
    of
      Values ->
        [{_, State}| _] = Values,
        %% This fold combines two operations into a single pass:
        %%  - keep only values, and drop states
        %%  - reverse list
        ReversedValues = lists:foldl(fun ({Value,_}, Acc) ->
          case Value of
            nil -> Acc;
            _   -> [Value|Acc]
          end
        end, [], Values),

        (Bridge(ReversedValues))(State)
    catch
      true -> {}
    end
  end.

return(Value) ->
  fun (Input) ->
    {Value,Input}
  end.

zero() ->
  fun (_) ->
    {}
  end.

plus(A, B) ->
  fun (Input) ->
    case A(Input) of
      {}     -> B(Input);
      Result -> Result
    end
  end.

choice(Parsers) ->
  foldl(fun plus/2, zero(), Parsers).

item() ->
  fun (Input) ->
    case Input of
      []    -> {};
      [H|T] -> {H, T}
    end
  end.

satisfies(Predicate) ->
  bind(item(), fun (A) ->
    case Predicate(A) of
      true -> return(A);
      _    -> zero()
    end
  end).

many(Parser) ->
  plus(many1(Parser), return([])).

-monad(?MODULE).
many1(Parser) ->
  X  = Parser,
  Xs = many(Parser),
  return([X|Xs]).
  % bind(Parser, fun (X) ->
  %   bind(many(Parser), fun (Xs) ->
  %     return([X | Xs])
  %   end)
  % end).

char(A) -> satisfies(fun (B) -> A == B end).

-monad(?MODULE).
string([]) -> return([]);
string([C | Cs]) ->
  X  = char(C),
  Xs = string(Cs),
  return([X|Xs]).
  % bind(char(C), fun (X) ->
  %   bind(string(Cs), fun (Xs) ->
  %     return([X | Xs])
  %   end)
  % end).

in_seq(Start, End) ->
  satisfies(fun (Member) -> member(Member, lists:seq(Start, End)) end).

lower() -> in_seq($a, $z).
upper() -> in_seq($A, $Z).

-monad(?MODULE).
digit() ->
  N = in_seq($0, $9),
  return(N - $0).
  % bind(in_seq($0, $9), fun (N) ->
  %   return(N - $0)
  % end).
digits() ->
  Digits = many1(in_seq($0, $9)),
  return(list_to_integer(Digits)).
  % bind(many1(in_seq($0, $9)), fun (Digits) ->
  %   return(list_to_integer(Digits))
  % end).
letter() -> plus(lower(), upper()).
letters() -> many(letter()).
alpha_num() -> plus(letter(), digit()).

one_of(Seq) -> satisfies(fun (C) -> member(C, Seq) end).
none_of(Seq) -> satisfies(fun (C) -> not member(C, Seq) end).

one_of_strings(Strings) ->
  choice(map(fun string/1, Strings)).

space() -> one_of(" \v\f\t\r\n").
spaces() -> many(space()).

%% Parses zero or more Elements separated by Separator.
sep_by(Element, Separator) ->
  plus(sep_by1(Element, Separator), return([])).

%% Parses at least one Element or many separated by Separator.
sep_by1(Element, Separator) ->
  Elements = bind(Separator, fun (_) -> Element end),
  bind(Element, fun (X) ->
    bind(many(Elements), fun (Xs) ->
      return([X|Xs])
    end)
  end).

-monad(?MODULE).
between(Left, Middle, Right) ->
  _ = Left,
  M = Middle,
  _ = Right,
  return(M).
  % seq([Left, Middle, Right], fun ([_, M, _]) ->
  %   return(M)
  % end).

chain_left1(Parser, Operator) ->
  Rest = many(bind(Operator, fun (F) ->
    bind(Parser, fun (Y) ->
      return({F,Y})
    end)
  end)),

  bind(Parser, fun (P) ->
    bind(Rest, fun (FYs) ->
      return(lists:foldl(fun ({F,Y}, Acc) ->
        F(Acc, Y)
      end, P, FYs))
    end)
  end).
