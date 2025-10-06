-module(euler20_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ANSWER, 142913828922).

euler20_recursion_test() -> ?assertEqual(?ANSWER, recursion20:start()).

euler20_tail_recursion_test() -> ?assertEqual(?ANSWER, accRecursion20:start()).

euler20_modular_test() -> ?assertEqual(?ANSWER, svertka20:start()).

euler20_map_test() -> ?assertEqual(?ANSWER, otobraz20:start()).
