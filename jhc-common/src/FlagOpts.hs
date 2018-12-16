module FlagOpts(Flag(..),process,helpMsg,helpFlags) where

import qualified Data.Set as Set

-- | Flags
data Flag =
      BangPatterns      -- ^ - bang patterns
    | Boehm             -- ^ use Boehm garbage collector
    | Controlled        -- ^ with the '-f' flag, the following options are availible, you can
    | Cpp               -- ^ pass haskell source through c preprocessor
    | Debug             -- ^ enable debugging code in generated executable
    | Defaulting        -- ^ perform defaulting of ambiguous types
    | Exists            -- ^ exists keyword for existential types recognized
    | Ffi               -- ^ support foreign function declarations
    | Forall            -- ^ forall keyword for rank-n types and explicit quantification
    | FullInt           -- ^ extend Int and Word to 32 bits on a 32 bit machine (rather than 30)
    | GlobalOptimize    -- ^ perform whole program E optimization
    | InlinePragmas     -- ^ use inline pragmas
    | Jgc               -- ^ use the jgc garbage collector
    | Lint              -- ^ perform lots of extra type checks
    | M4                -- ^ pass haskell source through m4 preprocessor
    | MonomorphismRestriction -- ^ enforce monomorphism restriction
    | Negate            -- ^ any particular one by prepending 'no-' to it.
    | Prelude           -- ^ implicitly import Prelude
    | Profile           -- ^ enable profiling code in generated executable
    | Raw               -- ^ just evaluate main to WHNF and nothing else.
    | Rules             -- ^ use rules
    | Standalone        -- ^ compile to a standalone executable
    | Sugar             -- ^ disable all desugarings, only unboxed literals allowed.
    | TypeAnalysis      -- ^ perform a basic points-to analysis on types right after method generation
    | TypeFamilies      -- ^ type\/data family support
    | UnboxedTuples     -- ^ allow unboxed tuple syntax to be recognized
    | UnboxedValues     -- ^ allow unboxed value syntax
    | UserKinds         -- ^ user defined kinds
    | Wrapper           -- ^ wrap main in exception handler
    | Never             -- ^ Will never be set
    deriving(Eq,Ord,Bounded)

instance Show Flag where
    show BangPatterns = "bang-patterns"
    show Boehm = "boehm"
    show Controlled = "controlled"
    show Cpp = "cpp"
    show Debug = "debug"
    show Defaulting = "defaulting"
    show Exists = "exists"
    show Ffi = "ffi"
    show Forall = "forall"
    show FullInt = "full-int"
    show GlobalOptimize = "global-optimize"
    show InlinePragmas = "inline-pragmas"
    show Jgc = "jgc"
    show Lint = "lint"
    show M4 = "m4"
    show MonomorphismRestriction = "monomorphism-restriction"
    show Negate = "negate"
    show Prelude = "prelude"
    show Profile = "profile"
    show Raw = "raw"
    show Rules = "rules"
    show Standalone = "standalone"
    show Sugar = "sugar"
    show TypeAnalysis = "type-analysis"
    show TypeFamilies = "type-families"
    show UnboxedTuples = "unboxed-tuples"
    show UnboxedValues = "unboxed-values"
    show UserKinds = "user-kinds"
    show Wrapper = "wrapper"
    show Never = "never"

one "bang-patterns" = Right $ Set.insert BangPatterns
one "no-bang-patterns" = Right $ Set.delete BangPatterns
one "boehm" = Right $ Set.insert Boehm
one "no-boehm" = Right $ Set.delete Boehm
one "controlled" = Right $ Set.insert Controlled
one "no-controlled" = Right $ Set.delete Controlled
one "cpp" = Right $ Set.insert Cpp
one "no-cpp" = Right $ Set.delete Cpp
one "debug" = Right $ Set.insert Debug
one "no-debug" = Right $ Set.delete Debug
one "default" = Right $ foldr (.) id [ f | Right f <- [ one "inline-pragmas",one "rules",one "wrapper",one "defaulting",one "type-analysis",one "monomorphism-restriction",one "global-optimize",one "full-int",one "prelude",one "sugar"]]
one "defaulting" = Right $ Set.insert Defaulting
one "no-defaulting" = Right $ Set.delete Defaulting
one "exists" = Right $ Set.insert Exists
one "no-exists" = Right $ Set.delete Exists
one "ffi" = Right $ Set.insert Ffi
one "no-ffi" = Right $ Set.delete Ffi
one "forall" = Right $ Set.insert Forall
one "no-forall" = Right $ Set.delete Forall
one "full-int" = Right $ Set.insert FullInt
one "no-full-int" = Right $ Set.delete FullInt
one "glasgow-exts" = Right $ foldr (.) id [ f | Right f <- [ one "forall",one "ffi",one "unboxed-tuples"]]
one "global-optimize" = Right $ Set.insert GlobalOptimize
one "no-global-optimize" = Right $ Set.delete GlobalOptimize
one "inline-pragmas" = Right $ Set.insert InlinePragmas
one "no-inline-pragmas" = Right $ Set.delete InlinePragmas
one "jgc" = Right $ Set.insert Jgc
one "no-jgc" = Right $ Set.delete Jgc
one "lint" = Right $ Set.insert Lint
one "no-lint" = Right $ Set.delete Lint
one "m4" = Right $ Set.insert M4
one "no-m4" = Right $ Set.delete M4
one "monomorphism-restriction" = Right $ Set.insert MonomorphismRestriction
one "no-monomorphism-restriction" = Right $ Set.delete MonomorphismRestriction
one "negate" = Right $ Set.insert Negate
one "no-negate" = Right $ Set.delete Negate
one "prelude" = Right $ Set.insert Prelude
one "no-prelude" = Right $ Set.delete Prelude
one "profile" = Right $ Set.insert Profile
one "no-profile" = Right $ Set.delete Profile
one "raw" = Right $ Set.insert Raw
one "no-raw" = Right $ Set.delete Raw
one "rules" = Right $ Set.insert Rules
one "no-rules" = Right $ Set.delete Rules
one "standalone" = Right $ Set.insert Standalone
one "no-standalone" = Right $ Set.delete Standalone
one "sugar" = Right $ Set.insert Sugar
one "no-sugar" = Right $ Set.delete Sugar
one "type-analysis" = Right $ Set.insert TypeAnalysis
one "no-type-analysis" = Right $ Set.delete TypeAnalysis
one "type-families" = Right $ Set.insert TypeFamilies
one "no-type-families" = Right $ Set.delete TypeFamilies
one "unboxed-tuples" = Right $ Set.insert UnboxedTuples
one "no-unboxed-tuples" = Right $ Set.delete UnboxedTuples
one "unboxed-values" = Right $ Set.insert UnboxedValues
one "no-unboxed-values" = Right $ Set.delete UnboxedValues
one "user-kinds" = Right $ Set.insert UserKinds
one "no-user-kinds" = Right $ Set.delete UserKinds
one "wrapper" = Right $ Set.insert Wrapper
one "no-wrapper" = Right $ Set.delete Wrapper
one x = Left x

{-# NOINLINE process #-}
process s xs = foldr f (s,[]) (map one xs) where
   f (Right g) (s,xs) = (g s,xs)
   f (Left x) (s,xs) = (s,x:xs)

{-# NOINLINE helpMsg #-}
helpMsg = "\n-- Code options --\nbang-patterns   - bang patterns\ncpp             pass haskell source through c preprocessor\nexists          exists keyword for existential types recognized\nffi             support foreign function declarations\nforall          forall keyword for rank-n types and explicit\n                quantification\nm4              pass haskell source through m4 preprocessor\nprelude         implicitly import Prelude\nsugar           disable all desugarings, only unboxed literals allowed.\ntype-families   type/data family support\nunboxed-tuples  allow unboxed tuple syntax to be recognized\nunboxed-values  allow unboxed value syntax\nuser-kinds      user defined kinds\n\n-- Typechecking --\ndefaulting      perform defaulting of ambiguous types\nmonomorphism-restriction enforce monomorphism restriction\n\n-- Debugging --\nlint            perform lots of extra type checks\n\n-- Optimization Options --\nglobal-optimize perform whole program E optimization\ninline-pragmas  use inline pragmas\nrules           use rules\ntype-analysis   perform a basic points-to analysis on types right after\n                method generation\n\n-- Code Generation --\nboehm           use Boehm garbage collector\ndebug           enable debugging code in generated executable\nfull-int        extend Int and Word to 32 bits on a 32 bit machine\n                (rather than 30)\njgc             use the jgc garbage collector\nprofile         enable profiling code in generated executable\nraw             just evaluate main to WHNF and nothing else.\nstandalone      compile to a standalone executable\nwrapper         wrap main in exception handler\n\n-- Default settings --\ndefault         inline-pragmas rules wrapper defaulting type-analysis\n                monomorphism-restriction global-optimize full-int\n                prelude sugar\nglasgow-exts    forall ffi unboxed-tuples\n"
helpFlags = ["bang-patterns", "boehm", "controlled", "cpp", "debug", "default", "defaulting", "exists", "ffi", "forall", "full-int", "glasgow-exts", "global-optimize", "inline-pragmas", "jgc", "lint", "m4", "monomorphism-restriction", "negate", "prelude", "profile", "raw", "rules", "standalone", "sugar", "type-analysis", "type-families", "unboxed-tuples", "unboxed-values", "user-kinds", "wrapper"]
