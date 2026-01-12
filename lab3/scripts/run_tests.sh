#!/usr/bin/env bash
set -e

echo "Запуск тестов Erlang через run_tests.erl на GitHub Actions..."

# Переходим в корень lab3
cd "$(dirname "$0")/../"

# Компиляция run_tests.erl
erl -noshell -eval "compile:file(\"scripts/run_tests.erl\"), halt()."

# Запуск всех тестов через скомпилированный модуль
erl -noshell -pa scripts -s run_tests all
