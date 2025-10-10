# Лабораторная по функциональному программированию №1.
## Задача № 10.
The sum of the primes below 10 is 2+3+5+7=17. Find the sum of all the primes below two million. 

**Найти сумму всех простых чисел меньше 2,000,000.**

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


% Cделал хвостовой, иначе 2млн значений в стеке
is_prime(Num, Div) when Div * Div > Num -> true;
is_prime(Num, Div) when Num rem Div =:= 0 -> false;
is_prime(Num, Div) -> is_prime(Num, Div + 2).
```
## Модульное решение (генерация списков, фильтрация, свертка)
```erlang
start() -> folding(filtration(generating(3, 2000000)), 2).


generating(Start, Limit) -> [X || X<-lists:seq(Start,Limit, 2)].


filtration(Numbers) ->
  lists:filter(fun is_prime/1, Numbers).


folding(Numbers, Initial) ->
  lists:foldl(fun(N, Acc) -> Acc + N end, Initial, Numbers).


is_prime(N) -> is_prime(N, 3).
is_prime(N, D) when D * D > N -> true;
is_prime(N, D) when N rem D =:= 0 -> false;
is_prime(N, D) -> is_prime(N, D + 2).
```

## Решение с помощью отображения
```erlang
%Формально функция выполняет отображение
% start() ->  2 + lists:sum([X || X <- lists:seq(3, 2000000, 2), is_prime(X)]).


start() ->
      Seq = lists:seq(3, 2000000, 2),
      Sum = lists:sum(lists:map(fun(X) ->
        case is_prime(X) of
          true  -> X;
          false -> 0
        end
      end, Seq)),
      2 + Sum.


is_prime(N) -> is_prime(N, 3).
is_prime(N, D) when D * D > N -> true;
is_prime(N, D) when N rem D =:= 0 -> false;
is_prime(N, D) -> is_prime(N, D + 2).

```

## Решение на Java
```java
public class Main {
    public static void main(String[] args) {
        long sum = 0;
        for (int i = 2; i < 2_000_000; i++) {
            if (isPrime(i)) {
                sum += i;}
        }
        System.out.println(sum);}

    public static boolean isPrime(int n) {
        if (n < 2) return false;
        for (int i = 2; i * i <= n; i++) {
            if (n % i == 0) return false;}
        return true;}}
```

## Задача № 20.
For example, 10! = 10\*9\*8\*...\*1 = 3628800, and the sum of the digits in 10!is 3+6+2+8+8+0+0=27.
Find the sum of digits in 100!

**Найти сумму всех цифр в '100!'**

## Решение с помощью хвостовой рекурсии
```erlang
start() -> factorial(100).


factorial(N) -> factorial(N, 1).
factorial(0, Acc) -> num_splitter(Acc);
factorial(N, Acc) when N>0 ->  factorial(N-1, Acc*N).


num_splitter(N) -> num_splitter(N, []).
num_splitter(0, Digits) -> list_sum(Digits);
num_splitter(N, Digits) -> num_splitter(N div 10, [N rem 10 | Digits]).


list_sum(List) -> list_sum(0, List).
list_sum(Result, []) -> Result;
list_sum(Result, [H | T]) -> list_sum(Result + H, T).
```

## Решение с помощью рекурсии
```erlang
start() -> list_sum(num_splitter(factorial(100))).


factorial(0) -> 1;
factorial(N) when N>0 -> N* factorial(N-1).


num_splitter(0) -> [];
num_splitter(N) -> num_splitter(N div 10) ++ [N rem 10].


list_sum([]) -> 0;
list_sum([H|T]) -> list_sum(T) + H.
```



## Модульное решение (генерация списков, фильтрация, свертка)
```erlang
start() -> sum_folding(filtration(number_to_digits(multiplier_folding(generating())))).


generating() -> [X * (X + 1) || X <- lists:seq(1, 99, 2)].


multiplier_folding(List) ->
  lists:foldl(fun(Item, Acc) -> Acc * Item end, 1, List).


number_to_digits(Number) ->
  [C - $0 || C <- integer_to_list(Number)].


filtration(List) ->
  lists:filter(fun(Item) -> Item=/=0 end, List).


sum_folding(List) -> lists:foldl(fun(Item, Acc) -> Acc + Item end, 0, List).
```

## Решение с помощью отображения
```erlang
start() -> lists:sum(number_to_digits(factorial(100))).


factorial(N) -> factorial(N, 1).
factorial(0, Acc) -> Acc;
factorial(N, Acc) when N > 0 -> factorial(N - 1, Acc * N).


number_to_digits(N) ->
  DigitsString = integer_to_list(N),
  lists:map(fun(Char) ->
    Char - $0
            end, DigitsString).

```
## Решение на Java
```java
import java.math.BigInteger;

public class Main {
    public static void main(String[] args) {
        BigInteger factorial = BigInteger.ONE;

        for (int i = 2; i <= 100; i++) {
            factorial = factorial.multiply(BigInteger.valueOf(i));
        }

        String digits = factorial.toString();
        int sum = 0;
        for (char c : digits.toCharArray()) {
            sum += c - '0';
        }

        System.out.println(sum);
    }
}
```
