#!/usr/bin/env bash
set -ux
while sleep 1
do
  inotifywait --quiet -e modify -e delete . src src/*
  elm make --output=elm.js src/Main.elm
done
