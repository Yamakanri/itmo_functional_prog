#!/bin/bash
echo "Запуск всех тестов Erlang..."

erl -noshell -pa lab3/src -pa lab3/gen_server/src -pa lab3/test -pa lab3/gen_server/test \
    -eval '
        %% Компилируем все исходники
        [c(F) || F <- [
            "lab3/src/linear_interpolation.erl",
            "lab3/src/lab3_app.erl",
            "lab3/src/lagrange_interpolation.erl",
            "lab3/src/lab3_sup.erl",
            "lab3/src/main.erl",
            "lab3/src/interpolation.erl",
            "lab3/src/io_server.erl",
            "lab3/gen_server/src/lab3_gen_server_sup.erl",
            "lab3/gen_server/src/lab3_gen_server_app.erl",
            "lab3/gen_server/src/lagrange_server.erl",
            "lab3/gen_server/src/interpolation_sup.erl",
            "lab3/gen_server/src/output_server.erl",
            "lab3/gen_server/src/input_server.erl",
            "lab3/gen_server/src/interpolation_server.erl",
            "lab3/gen_server/src/linear_server.erl"
        ]],

        %% Компилируем тесты
        [c(F) || F <- [
            "lab3/test/proc_linear_interpolation_test.erl",
            "lab3/test/proc_parser_test.erl",
            "lab3/test/proc_points_generator_test.erl",
            "lab3/test/proc_lagrange_interpolation_test.erl",
            "lab3/gen_server/test/gen_server_interpolation_test.erl",
            "lab3/gen_server/test/gen_server_parser_test.erl"
        ]],

        %% Запускаем тесты
        Tests = [
            proc_linear_interpolation_test,
            proc_parser_test,
            proc_points_generator_test,
            proc_lagrange_interpolation_test,
            gen_server_interpolation_test,
            gen_server_parser_test
        ],
        lists:foreach(fun(M) -> io:format("\n=== Running ~p ===\n", [M]), eunit:test(M, [verbose]) end, Tests),
        halt().
    '
