#!/bin/bash

set -eu

NAME="scrapti-exe"
DIST_DIR="$(stack path --dist-dir)"
EXE="${DIST_DIR}/build/${NAME}/${NAME}"
REBUILD="${REBUILD:-0}"

if [[ $REBUILD == "1" ]] || [[ ! -f "${EXE}" ]]; then
  stack build --fast --test --no-run-tests
fi

exec ${EXE} $@
