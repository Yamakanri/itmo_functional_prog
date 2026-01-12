%% run_tests.erl
%% Модуль для запуска всех тестов
-module(run_tests).
-export([all/0]).

all() ->
  io:format("=== Компиляция исходников ===~n"),
  %% Компиляция исходников
  lists:foreach(fun(F) -> io:format("Компилируем ~s~n", [F]), compile:file(F) end,
    ["proc/src/linear_interpolation.erl",
      "proc/src/lab3_app.erl",
      "proc/src/lagrange_interpolation.erl",
      "proc/src/lab3_sup.erl",
      "proc/src/main.erl",
      "proc/src/interpolation.erl",
      "proc/src/io_server.erl",
      "gen_server/src/lab3_gen_server_sup.erl",
      "gen_server/src/lab3_gen_server_app.erl",
      "gen_server/src/lagrange_server.erl",
      "gen_server/src/interpolation_sup.erl",
      "gen_server/src/output_server.erl",
      "gen_server/src/input_server.erl",
      "gen_server/src/interpolation_server.erl",
      "gen_server/src/linear_server.erl"]),

  io:format("=== Компиляция тестов ===~n"),
  %% Компиляция тестов
  lists:foreach(fun(F) -> io:format("Компилируем тест ~s~n", [F]), compile:file(F) end,
    ["proc/test/proc_linear_interpolation_test.erl",
      "proc/test/proc_parser_test.erl",
      "proc/test/proc_points_generator_test.erl",
      "proc/test/proc_lagrange_interpolation_test.erl",
      "gen_server/test/gen_server_interpolation_test.erl",
      "gen_server/test/gen_server_parser_test.erl"]),

  %% Запуск тестов
  TestModules = [proc_linear_interpolation_test,
    proc_parser_test,
    proc_points_generator_test,
    proc_lagrange_interpolation_test,
    gen_server_interpolation_test,
    gen_server_parser_test],

  lists:foreach(fun(M) ->
    io:format("=== Running ~p ===~n", [M]),
    eunit:test(M, [verbose])
                end, TestModules),

  halt().
