-module(recursion10).
-export([start/0]).

start() ->
  %Перебираем с трех, двойка как простое число уже велючена в сумму
  2 + sum_primes(3, 2000000).


sum_primes(Current, Limit) when Current >= Limit -> 0;
sum_primes(Current, Limit) ->
  case is_prime(Current, 3) of
    true  -> Current + sum_primes(Current + 2, Limit);
    false -> sum_primes(Current + 2, Limit)
  end.

% Проверка на простоту - сделал хвостовой, иначе 2млн значений в стеке
is_prime(Num, Div) when Div * Div > Num ->
  true;
is_prime(Num, Div) when Num rem Div =:= 0 ->
  false;
is_prime(Num, Div) ->
  is_prime(Num, Div + 2).
