-module(proc_linear_interpolation_test).
-include_lib("eunit/include/eunit.hrl").

linear_interpolation_basic_test() ->
    % Тест линейной интерполяции с двумя точками
    Points = [[1.0, 2.0], [3.0, 6.0]],
    Step = 0.5,
    Result = linear_interpolation:linear_interpolation(Step, Points),
    ?assertMatch({ok, linear, _}, Result),
    {ok, linear, [GeneratedDots, InterpolatedValues]} = Result,
    % Проверяем, что сгенерированы точки
    ?assert(length(GeneratedDots) > 0),
    % Проверяем, что количество точек и значений совпадает
    ?assertEqual(length(GeneratedDots), length(InterpolatedValues)).

linear_interpolation_same_x_test() ->
    % Тест ошибки при одинаковых X координатах
    Points = [[1.0, 2.0], [1.0, 3.0]],
    Step = 0.5,
    Result = linear_interpolation:linear_interpolation(Step, Points),
    ?assertMatch({error, linear}, Result).

linear_interpolation_values_test() ->
    % Тест, что интерполяция дает правильные значения в исходных точках
    Points = [[0.0, 0.0], [2.0, 4.0]],
    Step = 1.0,
    {ok, linear, [GeneratedDots, InterpolatedValues]} = 
        linear_interpolation:linear_interpolation(Step, Points),
    % Первая точка должна быть (0.0, 0.0)
    ?assert(lists:member(0.0, GeneratedDots)),
    FirstIndex = find_index(0.0, GeneratedDots),
    ?assertEqual(0.0, lists:nth(FirstIndex, InterpolatedValues)),
    % Последняя точка должна быть (2.0, 4.0)
    ?assert(lists:member(2.0, GeneratedDots)),
    LastIndex = find_index(2.0, GeneratedDots),
    ?assertEqual(4.0, lists:nth(LastIndex, InterpolatedValues)).

find_index(Value, List) ->
    find_index(Value, List, 1).
find_index(Value, [Value | _], Index) -> Index;
find_index(Value, [_ | Rest], Index) -> find_index(Value, Rest, Index + 1).

