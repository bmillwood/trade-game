#!/usr/bin/env bash
set -ux

function waitForSource() {
  inotifywait --quiet -e modify -e delete .
}

function run() {
  cabal run
  echo "cabal run exited $?"
}

while sleep 1
do
  if cabal build
  then
    run &
    waitForSource
    kill %%
  else
    waitForSource
  fi
done
