-module(euler10_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ANSWER, 142913828922).

euler10_recursion_test() -> ?assertEqual(?ANSWER, recursion10:start()).

euler10_tail_recursion_test() -> ?assertEqual(?ANSWER, accRecursion10:start()).

euler10_modular_test() -> ?assertEqual(?ANSWER, svertka10:start()).

euler10_map_test() -> ?assertEqual(?ANSWER, otobraz10:start()).
