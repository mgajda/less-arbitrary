#!/usr/bin/env bash

#stack test --profile --file-watch --ghc-options=-Wall --test-arguments="--fail-fast +RTS -xc"
stack test --file-watch --ghc-options=-Wall
#ghcid -c "stack ghci --profile union-types:test:spec" --test main --warnings
