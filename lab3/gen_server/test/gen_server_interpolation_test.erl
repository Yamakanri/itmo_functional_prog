-module(gen_server_interpolation_test).
-include_lib("eunit/include/eunit.hrl").

-export([
    points_generator_test/0,
    lagrange_multiplier_test/0,
    lagrange_polynomial_test/0,
    evaluate_lagrange_test/0
]).

%% Тест генератора точек
points_generator_test() ->
    Step = 0.5,
    X1 = 1.0,
    X2 = 3.0,
    Result = interpolation_server:points_generator(Step, X1, X2),
    ?assert(lists:member(1.0, Result)),
    ?assert(lists:member(2.0, Result)),
    ?assert(lists:member(3.0, Result)),
    ?assert(length(Result) > 0).

%% Тест множителя Лагранжа
lagrange_multiplier_test() ->
    Points = [[0.0, 1.0], [1.0, 2.0], [2.0, 3.0]],
    X = 0.5,
    Xi = 1.0,
    Result = lagrange_server:lagrange_multiplier(X, Xi, Points),
    ?assert(is_number(Result)).

%% Тест полинома Лагранжа
lagrange_polynomial_test() ->
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    X = 0.5,
    Result = lagrange_server:lagrange_polynomial(X, Points),
    ?assert(is_number(Result)),
    ?assert(Result >= 0.0),
    ?assert(Result =< 2.0).

%% Тест вычисления интерполяции Лагранжа
evaluate_lagrange_test() ->
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    Step = 0.5,
    Result = lagrange_server:evaluate_lagrange(Step, Points),
    ?assertMatch([_GeneratedDots, _InterpolatedValues], Result),
    [GeneratedDots, InterpolatedValues] = Result,
    ?assert(length(GeneratedDots) > 0),
    ?assertEqual(length(GeneratedDots), length(InterpolatedValues)).
