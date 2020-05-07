#!/usr/bin/env bash

stack test --profile --file-watch --ghc-options=-Wall --test-arguments="--fail-fast +RTS -xc"
#ghcid -c "stack ghci --profile union-types:test:spec" --test main --warnings
