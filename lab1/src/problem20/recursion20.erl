-module(recursion20).
-export([start/0]).

% Задание: Найти сумму всех цифр в числе 100!

start() -> list_sum(list_splitter(factorial(100))).

% Получение факториала переданного числа (100!)
factorial(0) -> 1;
factorial(N) when N>0 -> N * factorial(N - 1).

list_splitter(0) -> [];
list_splitter(N) -> list_splitter(N div 10) ++ [N rem 10].

list_sum([]) -> 0;
list_sum([H | T]) -> list_sum(T) + H.
