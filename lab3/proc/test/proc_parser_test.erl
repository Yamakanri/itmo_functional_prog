-module(proc_parser_test).
-include_lib("eunit/include/eunit.hrl").

convert_strings_to_atoms_test() ->
    % Тест конвертации списка строк в атомы
    Input = ["linear", "lagrange"],
    Expected = [linear, lagrange],
    ?assertEqual(Expected, main:convert_strings_to_atoms(Input)),
    % Тест пустого списка
    ?assertEqual([], main:convert_strings_to_atoms([])).

