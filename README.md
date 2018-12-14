# JHC Haskell Compiler

This is a fork of [JHC Haskell Compiler](http://repetae.net/computer/jhc/).

The source code is split into reusable components and builds with [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

### System Requirements
- OS: Windows/Linux/OSX

#### Compile & Run

To compile you will need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack build
stack exec -- jhc -h
```

To compiler the standard libraries:
```
stack exec -- jhc -L . --build-hl  lib/jhc-prim/jhc-prim.yaml
stack exec -- jhc -L . --build-hl  lib/jhc/jhc.yaml
stack exec -- jhc -L . --build-hl  lib/haskell-extras/haskell-extras.yaml
stack exec -- jhc -L . --build-hl  lib/haskell2010/haskell2010.yaml
stack exec -- jhc -L . --build-hl  lib/haskell98/haskell98.yaml
stack exec -- jhc -L . --build-hl  lib/applicative/applicative.yaml
stack exec -- jhc -L . --build-hl  lib/flat-foreign/flat-foreign.yaml
```

