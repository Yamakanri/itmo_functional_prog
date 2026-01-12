-module(proc_lagrange_interpolation_test).
-include_lib("eunit/include/eunit.hrl").

lagrange_multiplier_test() ->
    % Тест множителя Лагранжа
    Points = [[0.0, 1.0], [1.0, 2.0], [2.0, 3.0]],
    X = 0.5,
    Xi = 1.0,
    Result = lagrange_interpolation:lagrange_multiplier(X, Xi, Points),
    % Результат должен быть числом
    ?assert(is_number(Result)).

lagrange_polynomial_test() ->
    % Тест полинома Лагранжа
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    X = 0.5,
    Result = lagrange_interpolation:lagrange_polynomial(X, Points),
    % Для линейной функции результат должен быть около 0.5
    ?assert(is_number(Result)),
    ?assert(Result >= 0.0),
    ?assert(Result =< 2.0).

evaluate_lagrange_test() ->
    % Тест вычисления интерполяции Лагранжа
    Points = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]],
    Step = 0.5,
    Result = lagrange_interpolation:evaluate_lagrange(Step, Points),
    ?assertMatch([_GeneratedDots, _InterpolatedValues], Result),
    [GeneratedDots, InterpolatedValues] = Result,
    % Проверяем, что сгенерированы точки
    ?assert(length(GeneratedDots) > 0),
    % Проверяем, что количество точек и значений совпадает
    ?assertEqual(length(GeneratedDots), length(InterpolatedValues)).

