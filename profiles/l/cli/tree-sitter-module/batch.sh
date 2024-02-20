#!/usr/bin/env bash

set -u
set -e

languages=(
    'nix'
)

if [ -z "${JOBS:-}" ]
then
    for language in "${languages[@]}"
    do
        ./build.sh "${language}"
    done
else
    printf "%s\n" "${languages[@]}" | xargs -P"${JOBS}" -n1 ./build.sh
fi
