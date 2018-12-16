reset
set -x

rm -fr /home/csaba/.jhc

rm *.hl

stack exec -- jhc -L . --build-hl  lib/jhc-prim/jhc-prim.yaml
stack exec -- jhc -L . --build-hl  lib/jhc/jhc.yaml
stack exec -- jhc -L . --build-hl  lib/haskell-extras/haskell-extras.yaml
stack exec -- jhc -L . --build-hl  lib/haskell2010/haskell2010.yaml
stack exec -- jhc -L . --build-hl  lib/haskell98/haskell98.yaml
stack exec -- jhc -L . --build-hl  lib/applicative/applicative.yaml
stack exec -- jhc -L . --build-hl  lib/flat-foreign/flat-foreign.yaml

stack exec -- jhc -L . examples/Calendar.hs -o calendar
stack exec -- jhc -L . examples/HelloWorld.hs -o hello
stack exec -- jhc -L . examples/Primes.hs -o primes
