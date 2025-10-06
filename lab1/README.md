# Лабораторная по функциональному программированию №1.
## Задача № 10.
Найти сумму всех простых чисел меньше 2,000,000.

## Решение с помощью хвостовой рекурсии
```erlang
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
```erlang
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
```erlang
start() ->
  Candidates = generating(3, 2000000),
  PrimeNumbers = filtration(Candidates),
  folding(PrimeNumbers, 2).

generating(Start, Limit) ->
  [X|| X<-lists:seq(Start,Limit, 2)].

filtration(Numbers) ->
  lists:filter(fun is_prime/1, Numbers).

folding(Numbers, Initial) ->
  lists:foldl(fun(Numbers, Acc) -> Acc + Numbers end, Initial, Numbers).


is_prime(2) -> true;
is_prime(Num) -> is_prime(Num, 3).
is_prime(Num, Div) when Div * Div > Num -> true;
is_prime(Num, Div) when Num rem Div =:= 0 -> false;
is_prime(Num, Div) -> is_prime(Num, Div + 2).
```



## Задача № 20.
Найти сумму всех цифр в числе '100!'

## Решение с помощью хвостовой рекурсии
```erlang
start() -> fact(100).

fact(N) -> fact(N, 1).

fact(0, Acc) -> split(Acc);
fact(N, Acc) when N>0 ->  fact(N-1, Acc*N).


split(Num) -> split(Num, []).

split(0, Digits) ->sum(Digits);
split(Num, Digits) -> split(Num div 10, [Num rem 10 | Digits]).


sum(List) -> sum(0, List).

sum(Result, []) -> Result;
sum(Result, [H | T]) -> sum(Result + H, T).
```

## Решение с помощью рекурсии
```erlang
start() -> sum(split(fact(100))).

fact(0) -> 1;
fact(N) when N>0 -> N*fact(N-1).

split(0) -> [];
split(N) -> split(N div 10) ++ [N rem 10].

sum([]) -> 0;
sum([H|T]) -> sum(T) + H.
```


#НА РЕМОНТЕ
## Модульное решение (генерация списков, фильтрация, свертка)
**(Есть версия лаконичнее, но без генерации списков, кроме такого варианта ее просто не впихнуть)**
```erlang
start() ->
  FactorialSums = generator(),
  FactorialResult = svertka(FactorialSums),
  SplittedList = number_to_digits(FactorialResult),
  FiltratedList = filtration(SplittedList),
  svertka2(FiltratedList).

generator() -> [X * (X + 1) || X <- lists:seq(1, 99, 2)].


svertka(List) ->
  lists:foldl(fun(Item, Acc) -> Acc * Item end, 1, List).

% Причина - integer_to_cherlist возвращает в ASCII
number_to_digits(Number) ->
  [C - $0 || C <- integer_to_list(Number)].

filtration(List) ->
  lists:filter(fun(Item) -> Item=/=0 end, List).

svertka2(List) 
```
