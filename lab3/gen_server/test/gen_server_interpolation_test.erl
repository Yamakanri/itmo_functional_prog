-module(gen_server_interpolation_test).
-include_lib("eunit/include/eunit.hrl").

points_generator_test() ->
    % Тест генератора точек
    Step = 0.5,
    X1 = 1.0,
    X2 = 3.0,
    Result = interpolation_server:points_generator(Step, X1, X2),
    % Проверяем, что результат содержит ожидаемые точки
    ?assert(lists:member(1.0, Result)),
    ?assert(lists:member(2.0, Result)),
    ?assert(lists:member(3.0, Result)),
    % Проверяем, что список не пустой
    ?assert(length(Result) > 0).

lagrange_multiplier_test() ->
    % Тест множителя Лагранжа
    Points = [[0.0, 1.0], [1.0, 2.0], [2.0, 3.0]],
    X = 0.5,
    Xi = 1.0,
    Result = lagrange_server:lagrange_multiplier(X, Xi, Points),
    ?assert(is_number(Result)).

lagrange_polynomial_test() ->
    % Тест полинома Лагранжа
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    X = 0.5,
    Result = lagrange_server:lagrange_polynomial(X, Points),
    ?assert(is_number(Result)),
    ?assert(Result >= 0.0),
    ?assert(Result =< 2.0).

evaluate_lagrange_test() ->
    % Тест вычисления интерполяции Лагранжа
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    Step = 0.5,
    Result = lagrange_server:evaluate_lagrange(Step, Points),
    ?assertMatch([_GeneratedDots, _InterpolatedValues], Result),
    [GeneratedDots, InterpolatedValues] = Result,
    ?assert(length(GeneratedDots) > 0),
    ?assertEqual(length(GeneratedDots), length(InterpolatedValues)).

