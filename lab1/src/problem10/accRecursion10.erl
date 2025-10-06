-module(accRecursion10).
-export([start/0]).



start() ->
  %Перебираем с трех, двойка как простое число уже велючена в сумму
  sum_primes(3, 2000000, 2).


sum_primes(Current, Limit, Sum) when Current >= Limit ->
  Sum;
sum_primes(Current, Limit, Sum) ->
  NewSum = case is_prime(Current, 3) of
             true  -> Sum + Current;
             false -> Sum
           end,
  sum_primes(Current + 2, Limit, NewSum).

% Проверка на простоту
is_prime(N, D) when D * D > N ->
  true;
is_prime(N, D) when N rem D =:= 0 ->
  false;
is_prime(N, D) ->
  is_prime(N, D + 2).
