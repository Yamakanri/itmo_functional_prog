%% lab3/scripts/run_tests.erl
-module(run_tests).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).

main() ->
    %% Список исходников
    Src = [
      "lab3/proc/src/linear_interpolation.erl",
      "lab3/proc/src/lab3_app.erl",
      "lab3/proc/src/lagrange_interpolation.erl",
      "lab3/proc/src/lab3_sup.erl",
      "lab3/proc/src/main.erl",
      "lab3/proc/src/interpolation.erl",
      "lab3/proc/src/io_server.erl",
      "lab3/gen_server/src/lab3_gen_server_sup.erl",
      "lab3/gen_server/src/lab3_gen_server_app.erl",
      "lab3/gen_server/src/lagrange_server.erl",
      "lab3/gen_server/src/interpolation_sup.erl",
      "lab3/gen_server/src/output_server.erl",
      "lab3/gen_server/src/input_server.erl",
      "lab3/gen_server/src/interpolation_server.erl",
      "lab3/gen_server/src/linear_server.erl"
    ],

    %% Список тестов
    Tests = [
      "lab3/proc/test/proc_linear_interpolation_test.erl",
      "lab3/proc/test/proc_parser_test.erl",
      "lab3/proc/test/proc_points_generator_test.erl",
      "lab3/proc/test/proc_lagrange_interpolation_test.erl",
      "lab3/gen_server/test/gen_server_interpolation_test.erl",
      "lab3/gen_server/test/gen_server_parser_test.erl"
    ],

    %% Компилируем исходники
    lists:foreach(fun(F) -> io:format("Компилируем ~s~n", [F]), c(F) end, Src),

    %% Компилируем тесты
    lists:foreach(fun(F) -> io:format("Компилируем тест ~s~n", [F]), c(F) end, Tests),

    %% Запускаем тесты
    AllTests = [list_to_atom(filename:basename(F, ".erl")) || F <- Tests],
    lists:foreach(fun(M) -> io:format("\n=== Running ~p ===\n", [M]), eunit:test(M, [verbose]) end, AllTests).
