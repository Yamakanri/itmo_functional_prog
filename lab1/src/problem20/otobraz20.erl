-module(otobraz20).
-export([start/0]).

start() -> lists:sum(number_to_digits(factorial(100))).

factorial(N) -> factorial(N, 1).
factorial(0, Acc) -> Acc;
factorial(N, Acc) when N > 0 -> factorial(N - 1, Acc * N).

number_to_digits(N) ->
  DigitsString = integer_to_list(N),
  lists:map(fun(Char) ->
    Char - $0
            end, DigitsString).

