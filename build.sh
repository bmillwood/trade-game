#!/usr/bin/env bash
set -ux

function waitForSource() {
  inotifywait --quiet -e modify -e delete bot protocol server
}

killGrandchild() {
  pkill --parent $(pgrep --parent $1)
}

while sleep 1
do
  if cabal build
  then
    cabal run trade-game-server -- server/static-root &
    waitForSource
    jobs -x killGrandchild %%
    wait
  else
    waitForSource
  fi
done
