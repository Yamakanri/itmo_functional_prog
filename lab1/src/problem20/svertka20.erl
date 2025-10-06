
-module(svertka20).

-export([start/0, number_to_digits/1]).

start() -> sum_folding(filtration(number_to_digits(multiplier_folding(generating())))).

generating() -> [X * (X + 1) || X <- lists:seq(1, 99, 2)].


multiplier_folding(List) ->
  lists:foldl(fun(Item, Acc) -> Acc * Item end, 1, List).

number_to_digits(Number) ->
  [C - 48 || C <- integer_to_list(Number)].

filtration(List) ->
  lists:filter(fun(Item) -> Item =/= 0 end, List).

sum_folding(List) -> lists:foldl(fun(Item, Acc) -> Acc + Item end, 0, List).
