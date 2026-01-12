#!/usr/bin/env bash

echo "Запуск тестов Erlang через run_tests.erl..."

erl -noshell -pa ./ebin -eval "
%% Компилируем run_tests.erl
c(\"lab3/scripts/run_tests.erl\"),

%% Запускаем функцию main
run_tests:main(),

halt().
"
