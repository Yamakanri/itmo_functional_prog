# Лабораторная по функциональному программированию №1.
## Задача № 10.
Найти сумму всех простых чисел меньше 2,000,000.

## Решение с помощью хвостовой рекурсии
```
start() ->
  sum_primes(3, 2000000, 2).

sum_primes(Current, Limit, Sum) when Current >= Limit ->
  Sum;
sum_primes(Current, Limit, Sum) ->
  NewSum = case is_prime(Current, 3) of
             true  -> Sum + Current;
             false -> Sum
           end,
  sum_primes(Current + 2, Limit, NewSum).

is_prime(N, D) when D * D > N ->
  true;
is_prime(N, D) when N rem D =:= 0 ->
  false;
is_prime(N, D) ->
  is_prime(N, D + 2).
```

## Решение с помощью рекурсии
```
start() ->
  2 + sum_primes(3, 2000000).

sum_primes(Current, Limit) when Current >= Limit ->
  0;
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
```
## Модульное решение (генерация списков, фильтрация, свертка)
```
start() ->
  Candidates = generator(3, 2000000),
  PrimeNumbers = filter(Candidates),
  svertka(PrimeNumbers, 2).

generator(Start, Limit) ->
  [X|| X<-lists:seq(Start,Limit, 2)].

filter(Numbers) ->
  lists:filter(fun is_prime/1, Numbers).

svertka(Numbers, Initial) ->
  lists:foldl(fun(Numbers, Acc) -> Acc + Numbers end, Initial, Numbers).


is_prime(2) -> true;
is_prime(N) -> is_prime(N, 3).
is_prime(Num, Div) when Div * Div > Num -> true;
is_prime(Num, Div) when Num rem Div =:= 0 -> false;
is_prime(Num, Div) -> is_prime(Num, Div + 2).
```
