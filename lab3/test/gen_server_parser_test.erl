-module(gen_server_parser_test).
-include_lib("eunit/include/eunit.hrl").

convert_strings_to_atoms_test() ->
    % Тест конвертации списка строк в атомы
    Input = ["linear", "lagrange"],
    Expected = [linear, lagrange],
    ?assertEqual(Expected, lab3_gen_server_app:strings_to_atoms(Input)),
    % Тест пустого списка
    ?assertEqual([], lab3_gen_server_app:strings_to_atoms([])).

parse_number_test() ->
    % Тест парсинга чисел
    ?assertEqual(42, lab3_gen_server_app:parse_number("42")),
    ?assertEqual(3.14, lab3_gen_server_app:parse_number("3.14")).

