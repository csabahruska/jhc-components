module FlagDump(Flag(..),process,helpMsg,helpFlags) where

import qualified Data.Set as Set

-- | Flags
data Flag =
      AllTypes          -- ^ show unified type table, after everything has been typechecked
    | Aspats            -- ^ show as patterns
    | Atom              -- ^ dump atom table on exit
    | Bindgroups        -- ^ show bindgroups
    | BoxySteps         -- ^ show step by step what the type inferencer is doing
    | C                 -- ^ don't delete C source file after compilation
    | Class             -- ^ detailed information on each class
    | ClassSummary      -- ^ summary of all classes
    | Core              -- ^ show intermediate core code
    | CoreAfterlift     -- ^ show final core before writing ho file
    | CoreBeforelift    -- ^ show core before lambda lifting
    | CoreInitial       -- ^ show core right after E.FromHs conversion
    | CoreMangled       -- ^ de-typed core right before it is converted to grin
    | CoreMini          -- ^ show details even when optimizing individual functions
    | CorePass          -- ^ show each iteration of code while transforming
    | CoreSteps         -- ^ show what happens in each pass
    | Datatable         -- ^ show data table of constructors
    | DatatableBuiltin  -- ^ show data table entries for some built in types
    | Dcons             -- ^ data constructors
    | Decls             -- ^ processed declarations before typechecking
    | Defs              -- ^ Show all defined names in a module
    | Derived           -- ^ show generated derived instances
    | EAlias            -- ^ show expanded aliases
    | EInfo             -- ^ show info tags on all bound variables
    | ESize             -- ^ print the size of E after each pass
    | EVerbose          -- ^ print very verbose version of E code always
    | Exports           -- ^ show which names are exported from each module
    | Fixity            -- ^ show fixity map
    | Grin              -- ^ dump all grin to the screen
    | GrinDatalog       -- ^ print out grin information in a format suitable for loading into a database
    | GrinFinal         -- ^ final grin before conversion to C
    | GrinGraph         -- ^ print dot file of final grin code to outputname_grin.dot
    | GrinInitial       -- ^ grin right after conversion from core
    | GrinNormalized    -- ^ grin right after first normalization
    | GrinPosteval      -- ^ show grin code just before eval\/apply inlining
    | GrinPreeval       -- ^ show grin code just before eval\/apply inlining
    | Imports           -- ^ show in scope names for each module
    | Ini               -- ^ all ini configuration options
    | Instance          -- ^ show instances
    | Kind              -- ^ show results of kind inference for each module
    | KindSteps         -- ^ show steps of kind inference
    | OptimizationStats -- ^ show combined stats of optimization passes
    | Parsed            -- ^ parsed code
    | Preprocessed      -- ^ code after preprocessing\/deliting
    | Program           -- ^ impl expls, the whole shebang.
    | Progress          -- ^ show basic progress indicators
    | Renamed           -- ^ code after uniqueness renaming
    | Rules             -- ^ show all user rules and catalysts
    | RulesSpec         -- ^ show specialization rules
    | SccModules        -- ^ show strongly connected modules in dependency order
    | Sigenv            -- ^ initial signature environment
    | Srcsigs           -- ^ processed signatures from source code
    | Stats             -- ^ show extra information about stuff
    | Steps             -- ^ show interpreter go
    | Tags              -- ^ list of all tags and their types
    | The               -- ^ '-d' flag. The following is a list of possible parameters you can pass to
    | Tokens            -- ^ raw token stream before parsing
    | Types             -- ^ display unified type table containing all defined names
    | Never             -- ^ Will never be set
    deriving(Eq,Ord,Bounded)

instance Show Flag where
    show AllTypes = "all-types"
    show Aspats = "aspats"
    show Atom = "atom"
    show Bindgroups = "bindgroups"
    show BoxySteps = "boxy-steps"
    show C = "c"
    show Class = "class"
    show ClassSummary = "class-summary"
    show Core = "core"
    show CoreAfterlift = "core-afterlift"
    show CoreBeforelift = "core-beforelift"
    show CoreInitial = "core-initial"
    show CoreMangled = "core-mangled"
    show CoreMini = "core-mini"
    show CorePass = "core-pass"
    show CoreSteps = "core-steps"
    show Datatable = "datatable"
    show DatatableBuiltin = "datatable-builtin"
    show Dcons = "dcons"
    show Decls = "decls"
    show Defs = "defs"
    show Derived = "derived"
    show EAlias = "e-alias"
    show EInfo = "e-info"
    show ESize = "e-size"
    show EVerbose = "e-verbose"
    show Exports = "exports"
    show Fixity = "fixity"
    show Grin = "grin"
    show GrinDatalog = "grin-datalog"
    show GrinFinal = "grin-final"
    show GrinGraph = "grin-graph"
    show GrinInitial = "grin-initial"
    show GrinNormalized = "grin-normalized"
    show GrinPosteval = "grin-posteval"
    show GrinPreeval = "grin-preeval"
    show Imports = "imports"
    show Ini = "ini"
    show Instance = "instance"
    show Kind = "kind"
    show KindSteps = "kind-steps"
    show OptimizationStats = "optimization-stats"
    show Parsed = "parsed"
    show Preprocessed = "preprocessed"
    show Program = "program"
    show Progress = "progress"
    show Renamed = "renamed"
    show Rules = "rules"
    show RulesSpec = "rules-spec"
    show SccModules = "scc-modules"
    show Sigenv = "sigenv"
    show Srcsigs = "srcsigs"
    show Stats = "stats"
    show Steps = "steps"
    show Tags = "tags"
    show The = "the"
    show Tokens = "tokens"
    show Types = "types"
    show Never = "never"

one "all-types" = Right $ Set.insert AllTypes
one "no-all-types" = Right $ Set.delete AllTypes
one "aspats" = Right $ Set.insert Aspats
one "no-aspats" = Right $ Set.delete Aspats
one "atom" = Right $ Set.insert Atom
one "no-atom" = Right $ Set.delete Atom
one "bindgroups" = Right $ Set.insert Bindgroups
one "no-bindgroups" = Right $ Set.delete Bindgroups
one "boxy-steps" = Right $ Set.insert BoxySteps
one "no-boxy-steps" = Right $ Set.delete BoxySteps
one "c" = Right $ Set.insert C
one "no-c" = Right $ Set.delete C
one "class" = Right $ Set.insert Class
one "no-class" = Right $ Set.delete Class
one "class-summary" = Right $ Set.insert ClassSummary
one "no-class-summary" = Right $ Set.delete ClassSummary
one "core" = Right $ Set.insert Core
one "no-core" = Right $ Set.delete Core
one "core-afterlift" = Right $ Set.insert CoreAfterlift
one "no-core-afterlift" = Right $ Set.delete CoreAfterlift
one "core-beforelift" = Right $ Set.insert CoreBeforelift
one "no-core-beforelift" = Right $ Set.delete CoreBeforelift
one "core-initial" = Right $ Set.insert CoreInitial
one "no-core-initial" = Right $ Set.delete CoreInitial
one "core-mangled" = Right $ Set.insert CoreMangled
one "no-core-mangled" = Right $ Set.delete CoreMangled
one "core-mini" = Right $ Set.insert CoreMini
one "no-core-mini" = Right $ Set.delete CoreMini
one "core-pass" = Right $ Set.insert CorePass
one "no-core-pass" = Right $ Set.delete CorePass
one "core-steps" = Right $ Set.insert CoreSteps
one "no-core-steps" = Right $ Set.delete CoreSteps
one "datatable" = Right $ Set.insert Datatable
one "no-datatable" = Right $ Set.delete Datatable
one "datatable-builtin" = Right $ Set.insert DatatableBuiltin
one "no-datatable-builtin" = Right $ Set.delete DatatableBuiltin
one "dcons" = Right $ Set.insert Dcons
one "no-dcons" = Right $ Set.delete Dcons
one "decls" = Right $ Set.insert Decls
one "no-decls" = Right $ Set.delete Decls
one "defs" = Right $ Set.insert Defs
one "no-defs" = Right $ Set.delete Defs
one "derived" = Right $ Set.insert Derived
one "no-derived" = Right $ Set.delete Derived
one "e-alias" = Right $ Set.insert EAlias
one "no-e-alias" = Right $ Set.delete EAlias
one "e-info" = Right $ Set.insert EInfo
one "no-e-info" = Right $ Set.delete EInfo
one "e-size" = Right $ Set.insert ESize
one "no-e-size" = Right $ Set.delete ESize
one "e-verbose" = Right $ Set.insert EVerbose
one "no-e-verbose" = Right $ Set.delete EVerbose
one "exports" = Right $ Set.insert Exports
one "no-exports" = Right $ Set.delete Exports
one "fixity" = Right $ Set.insert Fixity
one "no-fixity" = Right $ Set.delete Fixity
one "grin" = Right $ Set.insert Grin
one "no-grin" = Right $ Set.delete Grin
one "grin-datalog" = Right $ Set.insert GrinDatalog
one "no-grin-datalog" = Right $ Set.delete GrinDatalog
one "grin-final" = Right $ Set.insert GrinFinal
one "no-grin-final" = Right $ Set.delete GrinFinal
one "grin-graph" = Right $ Set.insert GrinGraph
one "no-grin-graph" = Right $ Set.delete GrinGraph
one "grin-initial" = Right $ Set.insert GrinInitial
one "no-grin-initial" = Right $ Set.delete GrinInitial
one "grin-normalized" = Right $ Set.insert GrinNormalized
one "no-grin-normalized" = Right $ Set.delete GrinNormalized
one "grin-posteval" = Right $ Set.insert GrinPosteval
one "no-grin-posteval" = Right $ Set.delete GrinPosteval
one "grin-preeval" = Right $ Set.insert GrinPreeval
one "no-grin-preeval" = Right $ Set.delete GrinPreeval
one "imports" = Right $ Set.insert Imports
one "no-imports" = Right $ Set.delete Imports
one "ini" = Right $ Set.insert Ini
one "no-ini" = Right $ Set.delete Ini
one "instance" = Right $ Set.insert Instance
one "no-instance" = Right $ Set.delete Instance
one "kind" = Right $ Set.insert Kind
one "no-kind" = Right $ Set.delete Kind
one "kind-steps" = Right $ Set.insert KindSteps
one "no-kind-steps" = Right $ Set.delete KindSteps
one "optimization-stats" = Right $ Set.insert OptimizationStats
one "no-optimization-stats" = Right $ Set.delete OptimizationStats
one "parsed" = Right $ Set.insert Parsed
one "no-parsed" = Right $ Set.delete Parsed
one "preprocessed" = Right $ Set.insert Preprocessed
one "no-preprocessed" = Right $ Set.delete Preprocessed
one "program" = Right $ Set.insert Program
one "no-program" = Right $ Set.delete Program
one "progress" = Right $ Set.insert Progress
one "no-progress" = Right $ Set.delete Progress
one "renamed" = Right $ Set.insert Renamed
one "no-renamed" = Right $ Set.delete Renamed
one "rules" = Right $ Set.insert Rules
one "no-rules" = Right $ Set.delete Rules
one "rules-spec" = Right $ Set.insert RulesSpec
one "no-rules-spec" = Right $ Set.delete RulesSpec
one "scc-modules" = Right $ Set.insert SccModules
one "no-scc-modules" = Right $ Set.delete SccModules
one "sigenv" = Right $ Set.insert Sigenv
one "no-sigenv" = Right $ Set.delete Sigenv
one "srcsigs" = Right $ Set.insert Srcsigs
one "no-srcsigs" = Right $ Set.delete Srcsigs
one "stats" = Right $ Set.insert Stats
one "no-stats" = Right $ Set.delete Stats
one "steps" = Right $ Set.insert Steps
one "no-steps" = Right $ Set.delete Steps
one "tags" = Right $ Set.insert Tags
one "no-tags" = Right $ Set.delete Tags
one "the" = Right $ Set.insert The
one "no-the" = Right $ Set.delete The
one "tokens" = Right $ Set.insert Tokens
one "no-tokens" = Right $ Set.delete Tokens
one "types" = Right $ Set.insert Types
one "no-types" = Right $ Set.delete Types
one "verbose" = Right $ foldr (.) id [ f | Right f <- [ one "progress"]]
one "veryverbose" = Right $ foldr (.) id [ f | Right f <- [ one "progress",one "stats"]]
one x = Left x

{-# NOINLINE process #-}
process s xs = foldr f (s,[]) (map one xs) where
   f (Right g) (s,xs) = (g s,xs)
   f (Left x) (s,xs) = (s,x:xs)

{-# NOINLINE helpMsg #-}
helpMsg = "\n-- Front End --\ndecls           processed declarations before typechecking\ndefs            Show all defined names in a module\nderived         show generated derived instances\nexports         show which names are exported from each module\nfixity          show fixity map\nimports         show in scope names for each module\nini             all ini configuration options\nparsed          parsed code\npreprocessed    code after preprocessing/deliting\nrenamed         code after uniqueness renaming\nscc-modules     show strongly connected modules in dependency order\ntokens          raw token stream before parsing\n\n-- Type Checker --\nall-types       show unified type table, after everything has been\n                typechecked\naspats          show as patterns\nbindgroups      show bindgroups\nboxy-steps      show step by step what the type inferencer is doing\nclass           detailed information on each class\nclass-summary   summary of all classes\ndcons           data constructors\ninstance        show instances\nkind            show results of kind inference for each module\nkind-steps      show steps of kind inference\nprogram         impl expls, the whole shebang.\nsigenv          initial signature environment\nsrcsigs         processed signatures from source code\ntypes           display unified type table containing all defined names\n\n-- Intermediate code --\ncore            show intermediate core code\ncore-afterlift  show final core before writing ho file\ncore-beforelift show core before lambda lifting\ncore-initial    show core right after E.FromHs conversion\ncore-mangled    de-typed core right before it is converted to grin\ncore-mini       show details even when optimizing individual functions\ncore-pass       show each iteration of code while transforming\ncore-steps      show what happens in each pass\ndatatable       show data table of constructors\ndatatable-builtin show data table entries for some built in types\ne-alias         show expanded aliases\ne-info          show info tags on all bound variables\ne-size          print the size of E after each pass\ne-verbose       print very verbose version of E code always\noptimization-stats show combined stats of optimization passes\nrules           show all user rules and catalysts\nrules-spec      show specialization rules\n\n-- Grin code --\ngrin            dump all grin to the screen\ngrin-datalog    print out grin information in a format suitable for\n                loading into a database\ngrin-final      final grin before conversion to C\ngrin-graph      print dot file of final grin code to\n                outputname_grin.dot\ngrin-initial    grin right after conversion from core\ngrin-normalized grin right after first normalization\ngrin-posteval   show grin code just before eval/apply inlining\ngrin-preeval    show grin code just before eval/apply inlining\nsteps           show interpreter go\ntags            list of all tags and their types\n\n-- Backend code --\nc               don't delete C source file after compilation\n\n-- Internal --\natom            dump atom table on exit\n\n-- General --\nprogress        show basic progress indicators\nstats           show extra information about stuff\nverbose         progress\nveryverbose     progress stats\n"
helpFlags = ["all-types", "aspats", "atom", "bindgroups", "boxy-steps", "c", "class", "class-summary", "core", "core-afterlift", "core-beforelift", "core-initial", "core-mangled", "core-mini", "core-pass", "core-steps", "datatable", "datatable-builtin", "dcons", "decls", "defs", "derived", "e-alias", "e-info", "e-size", "e-verbose", "exports", "fixity", "grin", "grin-datalog", "grin-final", "grin-graph", "grin-initial", "grin-normalized", "grin-posteval", "grin-preeval", "imports", "ini", "instance", "kind", "kind-steps", "optimization-stats", "parsed", "preprocessed", "program", "progress", "renamed", "rules", "rules-spec", "scc-modules", "sigenv", "srcsigs", "stats", "steps", "tags", "the", "tokens", "types", "verbose", "veryverbose"]
