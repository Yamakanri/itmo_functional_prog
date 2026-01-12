#!/bin/bash
set -e

echo "Компиляция run_tests.erl..."
erl -noshell -eval "compile:file(\"scripts/run_tests.erl\"), halt()."

echo "Запуск всех тестов..."
erl -noshell -pa scripts -s run_tests all
