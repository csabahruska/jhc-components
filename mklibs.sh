#!/bin/sh

reset
set -x

rm -fr "$HOME/.jhc/cache"

rm ./*.hl

if [ -d dist-newstyle ]; then
    echo "dist-newstyle folder detected; assuming Haskell Cabal"
    run_jhc() { cabal v2-run jhc -- "$@"; }
elif [ -d .stack-work ]; then
    echo ".stack-work folder detected; assuming Haskell Stack"
    run_jhc() { stack exec -- jhc "$@"; }
else
    echo "Neither Cabal nor Stack build folders detected. Run 'cabal v2-build jhc' or 'stack build' first."
    exit 1
fi

run_jhc -L . --build-hl lib/jhc-prim/jhc-prim.yaml
run_jhc -L . --build-hl lib/jhc/jhc.yaml
run_jhc -L . --build-hl lib/haskell-extras/haskell-extras.yaml
run_jhc -L . --build-hl lib/haskell2010/haskell2010.yaml
run_jhc -L . --build-hl lib/haskell98/haskell98.yaml
run_jhc -L . --build-hl lib/applicative/applicative.yaml
run_jhc -L . --build-hl lib/flat-foreign/flat-foreign.yaml

run_jhc -L . examples/Calendar.hs -o calendar
run_jhc -L . examples/HelloWorld.hs -o hello
run_jhc -L . examples/Primes.hs -o primes
