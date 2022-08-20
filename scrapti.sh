#!/bin/bash

set -eu

NAME="scrapti-exe"
DIST_DIR="$(stack path --dist-dir)"
EXE="${DIST_DIR}/build/${NAME}/${NAME}"

if [[ ! -f "${EXE}" ]]; then
  stack build --test --no-run-tests
fi

exec ${EXE} $@
