#!/usr/bin/env bash
set -ux

function waitForSource() {
  inotifywait --quiet -e modify -e delete .
}

while sleep 1
do
  if cabal build
  then
    cabal run &
    waitForSource
    kill %%
  else
    waitForSource
  fi
done
