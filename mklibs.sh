reset
set -x
rm -fr /home/csaba/.jhc

rm *.hl
stack exec -- jhc -L . --build-hl  lib/jhc-prim/jhc-prim.yaml
stack exec -- jhc -L . --build-hl  lib/jhc/jhc.yaml

stack exec -- jhc --build-hl  lib/applicative/applicative.yaml
#stack exec -- jhc --build-hl  lib/haskell98/haskell98.yaml
#stack exec -- jhc --build-hl  lib/flat-foreign/flat-foreign.yaml
#stack exec -- jhc --build-hl  lib/ext/utility-ht.yaml
#stack exec -- jhc --build-hl  lib/ext/Diff.yaml
#stack exec -- jhc --build-hl  lib/ext/deepseq.yaml
#stack exec -- jhc --build-hl  lib/ext/bytestring.yaml
#stack exec -- jhc --build-hl  lib/ext/transformers.yaml
#stack exec -- jhc --build-hl  lib/ext/dataenc.yaml
#stack exec -- jhc --build-hl  lib/ext/QuickCheck.yaml
#stack exec -- jhc --build-hl  lib/ext/pretty.yaml
#stack exec -- jhc --build-hl  lib/ext/parsec.yaml
#stack exec -- jhc --build-hl  lib/ext/html.yaml
#stack exec -- jhc --build-hl  lib/ext/filepath.yaml
#stack exec -- jhc --build-hl  lib/ext/containers.yaml
#stack exec -- jhc --build-hl  lib/ext/binary.yaml
#stack exec -- jhc --build-hl  lib/ext/xhtml.yaml
#stack exec -- jhc --build-hl  lib/ext/smallcheck.yaml
#stack exec -- jhc --build-hl  lib/ext/HUnit.yaml
#stack exec -- jhc --build-hl  lib/ext/safe.yaml
