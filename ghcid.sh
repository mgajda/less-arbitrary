#!/usr/bin/env bash

stack test --profile --file-watch --test-arguments="--fail-fast +RTS -xc"
#ghcid -c "stack ghci --profile union-types:test:spec" --test main --warnings
