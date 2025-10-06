-module(accRecursion20).
-export([start/0]).

% Задание: Найти сумму всех цифр в числе 100"
start() -> factorial(100).

% Получение факториала переданного числа (100!)
factorial(N) -> factorial(N, 1).

factorial(0, Acc) -> num_splitter(Acc);
factorial(N, Acc) when N>0 -> factorial(N - 1, Acc * N).

%Разбиение числа на массив
num_splitter(N) -> num_splitter(N, []).

num_splitter(0, Digits) -> list_sum(Digits);
num_splitter(N, Digits) -> num_splitter(N div 10, [N rem 10 | Digits]).

% Сложение элементов массива
list_sum(List) -> list_sum(0, List).

list_sum(Result, []) -> Result;
list_sum(Result, [H | T]) -> list_sum(Result + H, T).
