-module(proc_points_generator_test).
-include_lib("eunit/include/eunit.hrl").

points_generator_basic_test() ->
    % Тест генератора точек
    Step = 0.5,
    X1 = 1.0,
    X2 = 3.0,
    Result = interpolation:points_generator(Step, X1, X2),
    % Проверяем, что результат содержит ожидаемые точки
    ?assert(lists:member(1.0, Result)),
    ?assert(lists:member(2.0, Result)),
    ?assert(lists:member(3.0, Result)),
    % Проверяем, что все точки в правильном диапазоне
    ?assert(lists:all(fun(X) -> X >= X1 end, Result)),
    % Проверяем, что список не пустой
    ?assert(length(Result) > 0).

points_generator_single_point_test() ->
    % Тест, когда начальная и конечная точки близки
    Step = 0.5,
    X1 = 1.0,
    X2 = 1.0,
    Result = interpolation:points_generator(Step, X1, X2),
    % Должна быть хотя бы начальная точка
    ?assert(length(Result) > 0),
    ?assert(lists:member(X1, Result)).

points_generator_empty_test() ->
    % Тест для некорректных входных данных
    Step = 0.5,
    X1 = 3.0,
    X2 = 1.0,  % X2 < X1
    Result = interpolation:points_generator(Step, X1, X2),
    ?assertEqual([], Result).

