-module(gen_server_parser_test).
-include_lib("eunit/include/eunit.hrl").

-export([
    convert_strings_to_atoms_test/0,
    parse_number_test/0
]).

%% Тест конвертации списка строк в атомы
convert_strings_to_atoms_test() ->
    Input = ["linear", "lagrange"],
    Expected = [linear, lagrange],
    ?assertEqual(Expected, lab3_gen_server_app:strings_to_atoms(Input)),
    ?assertEqual([], lab3_gen_server_app:strings_to_atoms([])).

%% Тест парсинга чисел
parse_number_test() ->
    ?assertEqual(42, lab3_gen_server_app:parse_number("42")),
    ?assertEqual(3.14, lab3_gen_server_app:parse_number("3.14")).
