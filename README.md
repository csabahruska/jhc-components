# JHC Haskell Compiler

This is a fork of [JHC Haskell Compiler](http://repetae.net/computer/jhc/) 0.8.2.

The source code is split into reusable components and builds with [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

### Components
- `jhc-compat`: Haskell98 compatibility library
- `jhc-common`: Compiler foundation library
- `jhc-frontend`: Haskell Front-End, lexer, parser, type-checker
- `jhc-core`: Core language, based on [Pure Type System](https://en.wikipedia.org/wiki/Pure_type_system)
- `jhc-grin`: Backend, C code generator, whole program optimizer, variant of GRIN intermediate language
- `jhc-app`: JHC Compiler, command line interface

NOTE: *lib* contains the standard Haskell libraries for JHC

### System Requirements
- OS: Windows/Linux/OSX
- Tools: [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) or [Haskell Cabal](https://www.haskell.org/cabal/)

### Compile & Run

#### Stack
The instructions below are for [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack build
stack exec -- jhc --help
```

To compile the standard libraries run:
```
stack exec -- jhc -L . --build-hl  lib/jhc-prim/jhc-prim.yaml
stack exec -- jhc -L . --build-hl  lib/jhc/jhc.yaml
stack exec -- jhc -L . --build-hl  lib/haskell-extras/haskell-extras.yaml
stack exec -- jhc -L . --build-hl  lib/haskell2010/haskell2010.yaml
stack exec -- jhc -L . --build-hl  lib/haskell98/haskell98.yaml
stack exec -- jhc -L . --build-hl  lib/applicative/applicative.yaml
stack exec -- jhc -L . --build-hl  lib/flat-foreign/flat-foreign.yaml
```

To compile the examples run:
```
stack exec -- jhc -L . examples/Calendar.hs -o calendar
stack exec -- jhc -L . examples/HelloWorld.hs -o hello
stack exec -- jhc -L . examples/Primes.hs -o primes
```

#### Cabal
The instructions below are for [Haskell Cabal](https://www.haskell.org/cabal/) version 2.4.1 or later.

To (re)compile and run `jhc` from within the source-tree, simply invoke
```
cabal v2-run jhc -- --help
```

To compile the standard libraries run:
```
cabal v2-run jhc -- -L . --build-hl  lib/jhc-prim/jhc-prim.yaml
cabal v2-run jhc -- -L . --build-hl  lib/jhc/jhc.yaml
cabal v2-run jhc -- -L . --build-hl  lib/haskell-extras/haskell-extras.yaml
cabal v2-run jhc -- -L . --build-hl  lib/haskell2010/haskell2010.yaml
cabal v2-run jhc -- -L . --build-hl  lib/haskell98/haskell98.yaml
cabal v2-run jhc -- -L . --build-hl  lib/applicative/applicative.yaml
cabal v2-run jhc -- -L . --build-hl  lib/flat-foreign/flat-foreign.yaml
```

To compile the examples run:
```
cabal v2-run jhc -- -L . examples/Calendar.hs -o calendar
cabal v2-run jhc -- -L . examples/HelloWorld.hs -o hello
cabal v2-run jhc -- -L . examples/Primes.hs -o primes
```

### Development Ideas

Check the list of development [ideas](Ideas.md).
