#!/usr/bin/env bash
set -e

echo "Запуск всех тестов через Rebar3 EUnit..."

# Указываем путь к папке lab3
cd "$(dirname "$0")/../"

# Собираем и запускаем все тесты
rebar3 eunit -v
