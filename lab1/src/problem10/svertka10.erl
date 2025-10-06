
-module(svertka10).

-export([start/0]).


start() -> folding(filtration(generating(3, 2000000)), 2).

generating(Start, Limit) -> [X|| X<-lists:seq(Start,Limit, 2)].

filtration(Numbers) ->
  lists:filter(fun is_prime/1, Numbers).

folding(Numbers, Initial) ->
  lists:foldl(fun(N, Acc) -> Acc + N end, Initial, Numbers).

is_prime(N) -> is_prime(N, 3).
is_prime(N, D) when D * D > N -> true;
is_prime(N, D) when N rem D =:= 0 -> false;
is_prime(N, D) -> is_prime(N, D + 2).
