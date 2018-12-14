{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}

-- Since in general LALR parser generators produce horrible error messages, we
-- try to be as permissive as possible in this parser and then produce more
-- appropriate errors in the desugaring pass.

{
module FrontEnd.Lex.Parser (parse,parseDecls,parseStmt,parseModule) where

import Control.Monad.Error
import FrontEnd.HsSyn
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.ParseMonad
import FrontEnd.Lex.Post
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Info.Properties
import Name.Names
import qualified Data.Map as Map

}

%token

    '{' { L $$ LSpecial "{" }
    '}' { L $$ LSpecial "}" }
    ',' { L $$ LSpecial "," }
    ';' { L $$ LSpecial ";" }
    '[' { L $$ LSpecial "[" }
    ']' { L $$ LSpecial "]" }
    '`' { L $$ LSpecial "`" }
    '(' { L $$ LSpecial "(" }
    ')' { L $$ LSpecial ")" }
    '(#' { L $$ LSpecial "(#" }
    '#)' { L $$ LSpecial "#)" }
    '#-}' { L $$ LSpecial "#-}" }
    'case' { L $$ LReservedId "case" }
    'class' { L $$ LReservedId "class" }
    'data' { L $$ LReservedId "data" }
    'default' { L $$ LReservedId "default" }
    'deriving' { L $$ LReservedId "deriving" }
    'do' { L $$ LReservedId "do" }
    'else' { L $$ LReservedId "else" }
    'if' { L $$ LReservedId "if" }
    'import' { L $$ LReservedId "import" }
    'in' { L $$ LReservedId "in" }
    'infix' { L $$ LReservedId "infix" }
    'infixl' { L $$ LReservedId "infixl" }
    'infixr' { L $$ LReservedId "infixr" }
    'instance' { L $$ LReservedId "instance" }
    'let' { L $$ LReservedId "let" }
    'module' { L $$ LReservedId "module" }
    'newtype' { L $$ LReservedId "newtype" }
    'of' { L $$ LReservedId "of" }
    'then' { L $$ LReservedId "then" }
    'type' { L $$ LReservedId "type" }
    'where' { L $$ LReservedId "where" }
    '_' { L $$ LReservedId "_" }
--#map { token('LReservedId',$_) } qw/as hiding qualified/
    'kind' { L $$ LReservedId "kind" }
    'alias' { L $$ LReservedId "alias" }
    'prefixx' { L $$ LReservedId "prefixx" }
    'prefixy' { L $$ LReservedId "prefixy" }
    'forall' { L $$ LReservedId "forall" }
    'exists' { L $$ LReservedId "exists" }
    'family' { L $$ LReservedId "family" }
    'closed' { L $$ LReservedId "closed" }
    'foreign' { L $$ LReservedId "foreign" }
-- #map { token('LReservedOp',$_) } qw/.. : :: = \\\\ | <- -> @ ~ =>/
    '..' { L $$ LReservedOp ".." }
    '::' { L $$ LReservedOp "::" }
    '=' { L $$ LReservedOp "=" }
    '\\' { L $$ LReservedOp "\\" }
    '|' { L $$ LReservedOp "|" }
    '<-' { L $$ LReservedOp "<-" }
    '->' { L $$ LReservedOp "->" }
    '=>' { L $$ LReservedOp "=>" }

    'NOINLINE' { L _ LPragmaStart $$@"NOINLINE" }
    'CATALYST' { L _ LPragmaStart $$@"CATALYST" }
    'SPECIALIZE' { L _ LPragmaStart $$@"SPECIALIZE" }
    'MULTISPECIALIZE' { L _ LPragmaStart $$@"MULTISPECIALIZE" }
    'SUPERSPECIALIZE' { L _ LPragmaStart $$@"SUPERSPECIALIZE" }
    'RULE' { L _ LPragmaStart $$@"RULE" }
    'NOETA' { L _ LPragmaStart $$@"NOETA" }
    'SUPERINLINE' { L _ LPragmaStart $$@"SUPERINLINE" }
    'CTYPE' { L _ LPragmaStart $$@"CTYPE" }
    'INLINE' { L _ LPragmaStart $$@"INLINE" }
    'SRCLOC_ANNOTATE' { L _ LPragmaStart $$@"SRCLOC_ANNOTATE" }
 '.' { L $$ LVarSym "." }
 'as' { L $$ LVarId "as" }
 FORALL { L $$ LVarId "forall" }   -- used inside of rules when -fno-forall is in effect
 'hiding' { L $$ LVarId "hiding" }
 'qualified' { L $$ LVarId "qualified" }

 cons_id  { L $$ LVarId ":" }
 cons_sym { L $$ LVarSym ":" }

  LVarId   { L _ LVarId $$ }
  LQVarId  { L _ LQVarId $$ }
  LConId   { L _ LConId $$ }
  LQConId  { L _ LQConId $$ }
  LVarSym  { L _ LVarSym $$ }
  LQVarSym { L _ LQVarSym $$ }
  LConSym  { L _ LConSym $$ }
  LQConSym { L _ LQConSym $$ }

  LInteger  { L _ LInteger $$ }
  LInteger_ { L _ LInteger_ $$ }
  LFloat    { L _ LFloat $$ }
  LFloat_    { L _ LFloat_ $$ }
  LChar     { L _ LChar $$ }
  LChar_     { L _ LChar_ $$ }
  LString   { L _ LString $$ }
  LString_  { L _ LString_ $$ }

%monad { P } { (>>=) } { return }
%name parse exp
%name parseDecls decls
%name parseStmt stmt
%name parseModule module
%tokentype { Lexeme }
%%

-- some combinators for generating rules



module :: { HsModule }
    : '{' impdecls decls '}'  { hsModule {
        hsModuleName    = mod_Main,
        hsModuleExports = Just [HsEVar vu_main],
        hsModuleSrcLoc  = $1,
        hsModuleImports = $2,
        hsModuleDecls   = fixupHsDecls $3
    } }
    | 'module' modid m_exports 'where' '{' impdecls decls '}' { hsModule {
        hsModuleName    = $2,
        hsModuleExports = $3,
        hsModuleSrcLoc  = $1,
        hsModuleImports = $6,
        hsModuleDecls   = fixupHsDecls $7
        } }

m_exports : exports  { Just $1 }
   |  { Nothing }

pats :: { [HsPat] }
    : srcloc exp0 {% withSrcLoc $1 (checkPatterns $2) }

epat :: { HsPat }
    : exp {% checkPattern $1 }

CTYPE :: { String } :  'CTYPE' STRING '#-}'  { $2 }
m_CTYPE : CTYPE  { Just $1 }
   |  { Nothing }

decl :: { HsDecl }
    : exp0 srcloc rhs optwhere  {% checkValDef $2 $1 $3 $4 }
    | cl_var '::' qualtype { HsTypeSig $2 $1 $3 }
    | 'type' con ewl_atype '=' type
                      { HsTypeDecl {
                        hsDeclSrcLoc = $1,
                        hsDeclName = nameTyLevel_u (const typeLevel) $2,
                        hsDeclTArgs = $3,
                        hsDeclType = $5 } }
    | assoc INT cl_varconop  { HsInfixDecl (fst $1) (snd $1) $2 $3 }
    | 'data' m_CTYPE qualtype mconstrs deriving {% withSrcLoc $1 $ do
        (cs,c,t) <- checkDataHeader $3
        return hsDataDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $5,
            hsDeclCons = fst $4, hsDeclHasKind = snd $4, hsDeclCTYPE = $2 } }
    | 'newtype' m_CTYPE qualtype '=' constr deriving {% withSrcLoc $1 $ do
        (cs,c,t) <- checkDataHeader $3
        return hsNewTypeDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $6,
            hsDeclCons = [$5], hsDeclCTYPE = $2 } }
    | 'instance' classhead optwhere { HsInstDecl $1 $2 $3 }
    | 'class' classhead optwhere { HsClassDecl $1 $2 $3 }
    | 'foreign' 'import' ewl_var mstring '::' qualtype
                    {% doForeign $1 (vu_import:$3) $4 $6  }
    | 'foreign' wl_var mstring '::' qualtype {% doForeign $1 $2 $3 $5  }
    | propspragma srcloc m_slist ecl_var '#-}'  { HsPragmaProps $2 $1 $4 }
    | 'deriving' 'instance' classhead { HsDeclDeriving $1 $3 }
    | 'default' type { HsDefaultDecl $1 $2 }
    | rulecatalyst m_slist rules '#-}' {
        HsPragmaRules $ map (\x -> x { hsRuleIsMeta = $1 }) ($3) }
    | srcloc specialize m_con var '::' type '#-}'
                      { HsPragmaSpecialize { hsDeclSrcLoc = $1, hsDeclBool = $2, hsDeclName = $4, hsDeclType = $6
                                           , hsDeclUniq = error "hsDeclUniq not set"  } }
    | srcloc specialize 'instance'  cl_type '#-}'
                      { HsPragmaSpecialize { hsDeclSrcLoc = $1, hsDeclBool = $2, hsDeclName = nameName u_instance , hsDeclType = head $4
                                           , hsDeclUniq = error "hsDeclUniq not set"  } }
m_con : con  { Just $1 }
   |  { Nothing }
m_slist : slist  { Just $1 }
   |  { Nothing }
m_semi : ';'  { Just $1 }
   |  { Nothing }

slist :: { [HsExp] }
    : '[' ecl_exp ']' { $2 }

rulecatalyst ::  { Bool }
    : 'RULE' { False }
    | 'CATALYST' { True }
specialize ::  { Bool }
    : 'SPECIALIZE' { False }
    | 'SUPERSPECIALIZE' { True }

rule :: { HsRule } : srcloc STRING mfreevars exp '=' exp { HsRule {
    hsRuleSrcLoc = $1,
    hsRuleString = $2,
    hsRuleFreeVars = $3,
    hsRuleLeftExpr = $4,
    hsRuleRightExpr = $6,
    hsRuleUniq = error "hsRuleUniq not set",
    hsRuleIsMeta = error "hsRuleIsMeta not set" } }

mfreevars :: { [(HsName,Maybe HsType)] }
      : 'forall' vbinds '.' { $2 }
      | FORALL vbinds '.' { $2 }
      | { [] }

vbinds :: { [(HsName,Maybe HsType)] }
      : vbinds '(' var '::' type ')' { ($3,Just $5) : $1 }
      | vbinds var                   { ($2,Nothing) : $1 }
      |                              { [] }

propspragma :: { String }
    : 'INLINE' { $1 }
    | 'MULTISPECIALIZE' { $1 }
    | 'NOINLINE' { $1 }
    | 'SRCLOC_ANNOTATE' { $1 }
    | 'SUPERINLINE' { $1 }
    | 'NOETA' { $1 }

-- FFI parts
mstring :: { Maybe (String,Name) }
mstring : LString var    { Just (read $1,$2) }
        | {- empty -}    { Nothing }

classhead :: { HsClassHead }
    : qualtype {% qualTypeToClassHead $1 }

mconstrs :: { ([HsConDecl],Maybe HsKind) }
    : '=' bl_constr  { ($2,Nothing) }
    | '::' kind      { ([],Just $2) }
    |                { ([],Nothing) }

kind :: { HsKind }
      : bkind                          { $1 }
      | bkind '->' kind                { HsKindFn $1 $3 }

bkind :: { HsKind }
       : '(' kind ')'     { $2 }
       |  varop           {% toKindVarSym $1 }
       |  con             { HsKind $ nameTyLevel_s kindLevel $1 }

deriving :: { [Name] }
    : {- empty -}               { [] }
    | 'deriving' con            { [toName ClassName $2] }
    | 'deriving' '(' ')'        { [] }
    | 'deriving' '(' cl_con ')' { map (toName ClassName) $3 }

constr :: { HsConDecl }
    : srcloc mexists scontype   { HsConDecl {
        hsConDeclSrcLoc = $1,
        hsConDeclName = nameTyLevel_s termLevel (fst $3),
        hsConDeclConArg = (snd $3),
        hsConDeclExists = $2 } }
    | srcloc mexists gcon '{' ecl_fielddecl '}' { HsRecDecl {
        hsConDeclSrcLoc = $1,
        hsConDeclName = nameTyLevel_s termLevel $3,
        hsConDeclRecArg = $5,
        hsConDeclExists = $2 } }

fielddecl :: { ([HsName],HsBangType) }
    : cl_var '::' wl_batype  {% withSrcLoc $2 $ do
        tty <- checkBangType $3
        return (map (toName FieldLabel) $1, tty) }

mexists :: { [HsTyVarBind] }
        : 'exists' wl_tbind '.' { $2 }
        | 'forall' wl_tbind '.' { $2 }  -- Allowed for GHC compatability
        |                       { [] }

scontype :: { (HsName, [HsBangType]) }
    : wl_batype {% checkSconType $1  }

batype :: { Either Name HsType }
    : varconop    { Left $1 }
    | '->'        { Left tc_Arrow }
    | atype       { Right $1 }
    | quantifiedtype { Right $1 }

rhs :: { HsRhs }
    : '=' exp   { HsUnGuardedRhs $2 }
    | wl_gdrh   { HsGuardedRhss $1 }

gdrh :: { HsComp }
      : '|' exp '=' exp        { HsComp $1 [HsQualifier $2] $4 }

rhs_case :: { HsRhs }
    : '->' exp      { HsUnGuardedRhs $2 }
    | wl_gdrh_case  { HsGuardedRhss $1 }

gdrh_case :: { HsComp }
      : '|' exp '->' exp        { HsComp $1 [HsQualifier $2] $4 }

assoc :: { (SrcLoc,HsAssoc) }
    : 'infix'  { ($1,HsAssocNone) }
    | 'infixl'  { ($1,HsAssocLeft) }
    | 'infixr'  { ($1,HsAssocRight) }
    | 'prefixx'  { ($1,HsAssocPrefix) }
    | 'prefixy'  { ($1,HsAssocPrefixy) }

impdecl :: { HsImportDecl }
    : 'import' optqualified modid maybeas maybeimpspec { HsImportDecl $1 $3 $2 $4 $5 }

modid :: { Module }
    : LQConId { toModule $1 }
    | LConId { toModule $1 }

optqualified :: { Bool }
    : 'qualified'   { True  }
    | {- empty -}   { False }

maybeas :: { Maybe Module }
    : 'as' modid    { Just $2 }
    | {- empty -}   { Nothing }

maybeimpspec :: { Maybe (Bool, [HsExportSpec]) }
    : exports             { Just (False,$1) }
    | 'hiding' exports    { Just (True,$2) }
    | {- empty -}         { Nothing }

exports :: { [HsExportSpec] }
--    : '(' ')'                     { [] }
    : '(' ocl_export ')'  { $2 }

export :: { HsExportSpec }
    :  var                          { HsEVar $1 }
    |  con                          { HsEAbs $1 }
    |  con '(' '..' ')'             { HsEThingAll $1 }
    |  con '(' ecl_varcon ')'       { HsEThingWith $1 $3 }
    |  'module' modid               { HsEModuleContents $2 }

quantifiedtype :: { HsType }
    : 'forall' wl_tbind '.' qualtype  { HsTyForall { hsTypeVars = $2, hsTypeType = $4 } }
    | 'exists' wl_tbind '.' qualtype  { HsTyExists { hsTypeVars = $2, hsTypeType = $4 } }

type :: { HsType }
    : btype '->' type               { HsTyFun $1 $3 }
    | btype                         { $1 }
    | quantifiedtype                { $1 }

tbind :: { HsTyVarBind }
       : srcloc var                   { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = nameTyLevel_s typeLevel $2 } }
       | srcloc '(' var '::' kind ')' { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = nameTyLevel_s typeLevel $3, hsTyVarBindKind = Just $5 } }

btype :: { HsType }
    : btype atype                   { HsTyApp $1 $2 }
    | atype                         { $1 }

atype :: { HsType }
    : gcon                   { HsTyCon (nameTyLevel_s typeLevel $1) }
    | var                    { HsTyVar (nameTyLevel_s typeLevel $1) }
    | '(' '->' ')'           { HsTyCon $ quoteName tc_Arrow }
    | '(' ')'                { HsTyTuple [] }
    | '[' ']'                { HsTyCon $ quoteName tc_List }
    | '(' cl2_type ')'       { HsTyTuple $2 }
    | '(#' ecl_type '#)'     { HsTyUnboxedTuple $2 }
    | '[' type ']'           {% do
        return $ HsTyApp (HsTyCon $ quoteName tc_List) $2 }
    | '(' type ')'           { $2 }
    | '(' type '=' type ')'  { HsTyEq $2 $4 }

qualtype :: { HsQualType }
    : btype '=>' type   {% withSrcLoc $2 $ checkContext $1 >>= return . flip HsQualType $3 }
    | type             { HsQualType [] $1 }

exp :: { HsExp }
    : exp0 '::' qualtype { HsExpTypeSig $2 $1 $3 }
    | exp0               { $1 }

exp0  :: { HsExp }
    : exp1 { $1 }
    | aexp exp0 { $1 `cat` $2 }

exp1 :: { HsExp }
    : 'if' exp 'then' exp 'else' exp { HsIf (espan $1 $3 $ $2) (espan $3 $5 $4) (eloc $5 $6) }
    | '\\' pats '->' exp { HsLambda $1 $2 $4 }
    | 'let' '{' decls '}' 'in' exp { HsLet (fixupHsDecls $3) $6 }
    | 'case' exp 'of' '{' alts '}'  { espan $1 $6 $ HsCase (espan $1 $3 $2) $5 }
    | aexp  { $1 }

stmt :: { HsStmt }
    : epat '<-' exp      { HsGenerator $2 $1 $3 }
    | exp               { HsQualifier $1 }
    | 'let' '{' decls '}'    { HsLetStmt  (fixupHsDecls $3) }

alt :: { HsAlt }
    : srcloc epat rhs_case optwhere { HsAlt $1 $2 $3 $4 }
 --   : pat '->' exp { HsAlt $2 $1 (HsUnGuardedRhs $3) [] }

aexp :: { HsExp }
    : '(' ecl_exp ')'   {% do
        let ee = espan $1 $3
        case $2 of
            [x] -> return $ ee (HsParen x)
            []  -> return (HsCon $ quoteName dc_Unit)
            xs -> return $ ee $ HsTuple xs }
    | '(#' ecl_exp '#)' { espan $1 $3 $ HsUnboxedTuple $2 }
    | '[' list ']'      { espan $1 $3 $2 }
    | '_'               { HsWildCard $1 }
    | var               { HsVar $1 }
    | gcon              { HsCon $1 }
    | varop             { HsBackTick (HsVar $1) }
    | conop             { HsBackTick (HsCon $1) }
    | lit               { HsLit $1 }
    -- atomic after layout processing
    | 'do' '{' stmts  '}'    { HsDo $3 }
    | aexp '{' ecl_fbind '}' {% mkRecConstrOrUpdate $1 $3 }

m_comma :: { () } : ',' { () } |  { () }

commas :: { Int }
    : commas ','                    { $1 + 1 }
    | ','                           { 1 }

fbind :: { (Name,Maybe HsExp) }
    : uqvar '=' exp  { (toName FieldLabel $1,Just (eloc $2 $3)) }
    | uqvar { (toName FieldLabel $1,Nothing) }
    | '..'  { (u_DotDot,Nothing) }

optwhere :: { [HsDecl] }
    : 'where' '{' decls  '}'                { fixupHsDecls $3 }
    | {- empty -}                           { [] }

list :: { HsExp }
    : ecl_exp                 { HsList $1 }
    | exp '|' cl_stmt         { HsListComp HsComp { hsCompSrcLoc = $2, hsCompBody = $1, hsCompStmts = $3 }  }
    | cl_exp '..'             {% case $1 of
        [x]   -> return $ HsEnumFrom x
        [x,y] -> return $ HsEnumFromThen x y
        _ -> fail "parse error in list comprehension" }
    | cl_exp '..' exp         {% case $1 of
        [x]   -> return $ HsEnumFromTo x $3
        [x,y] -> return $ HsEnumFromThenTo x y $3
        _ -> fail "parse error in list comprehension" }

lit :: { HsLiteral }
    : LInteger  { HsInt $ read $1 }
    | LChar     { HsChar $ read $1 }
    | LString   { HsString $ read $1 }
    | LFloat    { HsFrac  $ toRational (read $1 :: Double) }

    | LChar_     { HsCharPrim $ readPrim $1 }
    | LFloat_    { HsFrac  $ toRational (readPrim $1 :: Double) }
    | LInteger_  { HsIntPrim $ readPrim $1 }
    | LString_   { HsStringPrim $ readPrim $1 }

STRING :: { String } : LString { read $1 }
INT :: { Int } : LInteger { read $1 }

rev_cl_exp
    : rev_cl_exp ',' exp  { $3:$1 }
    | exp { [$1] }

cl_exp : rev_cl_exp { reverse $1 }
ecl_exp
    : cl_exp { $1 }
    |       { [] }
cl2_exp
    : exp ',' cl_exp { $1:$3 }
rev_cl_stmt
    : rev_cl_stmt ',' stmt  { $3:$1 }
    | stmt { [$1] }

cl_stmt : rev_cl_stmt { reverse $1 }
ecl_stmt
    : cl_stmt { $1 }
    |       { [] }
cl2_stmt
    : stmt ',' cl_stmt { $1:$3 }
rev_cl_type
    : rev_cl_type ',' type  { $3:$1 }
    | type { [$1] }

cl_type : rev_cl_type { reverse $1 }
ecl_type
    : cl_type { $1 }
    |       { [] }
cl2_type
    : type ',' cl_type { $1:$3 }
rev_cl_var
    : rev_cl_var ',' var  { $3:$1 }
    | var { [$1] }

cl_var : rev_cl_var { reverse $1 }
ecl_var
    : cl_var { $1 }
    |       { [] }
cl2_var
    : var ',' cl_var { $1:$3 }
rev_cl_varcon
    : rev_cl_varcon ',' varcon  { $3:$1 }
    | varcon { [$1] }

cl_varcon : rev_cl_varcon { reverse $1 }
ecl_varcon
    : cl_varcon { $1 }
    |       { [] }
cl2_varcon
    : varcon ',' cl_varcon { $1:$3 }
rev_cl_varconop
    : rev_cl_varconop ',' varconop  { $3:$1 }
    | varconop { [$1] }

cl_varconop : rev_cl_varconop { reverse $1 }
ecl_varconop
    : cl_varconop { $1 }
    |       { [] }
cl2_varconop
    : varconop ',' cl_varconop { $1:$3 }
rev_cl_export
    : rev_cl_export ',' export  { $3:$1 }
    | export { [$1] }

cl_export : rev_cl_export { reverse $1 }
ecl_export
    : cl_export { $1 }
    |       { [] }
cl2_export
    : export ',' cl_export { $1:$3 }
rev_cl_con
    : rev_cl_con ',' con  { $3:$1 }
    | con { [$1] }

cl_con : rev_cl_con { reverse $1 }
ecl_con
    : cl_con { $1 }
    |       { [] }
cl2_con
    : con ',' cl_con { $1:$3 }
rev_cl_fielddecl
    : rev_cl_fielddecl ',' fielddecl  { $3:$1 }
    | fielddecl { [$1] }

cl_fielddecl : rev_cl_fielddecl { reverse $1 }
ecl_fielddecl
    : cl_fielddecl { $1 }
    |       { [] }
cl2_fielddecl
    : fielddecl ',' cl_fielddecl { $1:$3 }
rev_cl_fbind
    : rev_cl_fbind ',' fbind  { $3:$1 }
    | fbind { [$1] }

cl_fbind : rev_cl_fbind { reverse $1 }
ecl_fbind
    : cl_fbind { $1 }
    |       { [] }
cl2_fbind
    : fbind ',' cl_fbind { $1:$3 }
rev_bl_constr
    : rev_bl_constr '|' constr  { $3:$1 }
    | constr { [$1] }

bl_constr : rev_bl_constr { reverse $1 }
ebl_constr
    : bl_constr { $1 }
    |       { [] }
rev_wl_aexp : rev_wl_aexp aexp  { $2:$1 }
   | aexp { [$1] }
 wl_aexp : rev_wl_aexp { reverse $1 }
rev_wl_var : rev_wl_var var  { $2:$1 }
   | var { [$1] }
 wl_var : rev_wl_var { reverse $1 }
rev_wl_gdrh : rev_wl_gdrh gdrh  { $2:$1 }
   | gdrh { [$1] }
 wl_gdrh : rev_wl_gdrh { reverse $1 }
rev_wl_batype : rev_wl_batype batype  { $2:$1 }
   | batype { [$1] }
 wl_batype : rev_wl_batype { reverse $1 }
rev_wl_gdrh_case : rev_wl_gdrh_case gdrh_case  { $2:$1 }
   | gdrh_case { [$1] }
 wl_gdrh_case : rev_wl_gdrh_case { reverse $1 }
rev_wl_tbind : rev_wl_tbind tbind  { $2:$1 }
   | tbind { [$1] }
 wl_tbind : rev_wl_tbind { reverse $1 }
rev_ewl_aexp : rev_ewl_aexp aexp  { $2:$1 }
   | { [] }
 ewl_aexp : rev_ewl_aexp { reverse $1 }
rev_ewl_atype : rev_ewl_atype atype  { $2:$1 }
   | { [] }
 ewl_atype : rev_ewl_atype { reverse $1 }
rev_ewl_var : rev_ewl_var var  { $2:$1 }
   | { [] }
 ewl_var : rev_ewl_var { reverse $1 }


ocl_export : ocrev_export { reverse $1 }
ocrev_export
    : ocrev_export ',' export { $3 : $1 }
    | ocrev_export ','    { $1 }
    | export            { [$1] }
    | {- empty -}   { [] }
decls : rev_decl { reverse $1 }
rev_decl
    : rev_decl ';' decl { $3 : $1 }
    | rev_decl ';'    { $1 }
    | decl            { [$1] }
    | {- empty -}   { [] }
impdecls : rev_impdecl { reverse $1 }
rev_impdecl
    : rev_impdecl ';' impdecl { $3 : $1 }
    | rev_impdecl ';'    { $1 }
    | impdecl            { [$1] }
    | {- empty -}   { [] }
alts : rev_alt { reverse $1 }
rev_alt
    : rev_alt ';' alt { $3 : $1 }
    | rev_alt ';'    { $1 }
    | alt            { [$1] }
    | {- empty -}   { [] }
stmts : rev_stmt { reverse $1 }
rev_stmt
    : rev_stmt ';' stmt { $3 : $1 }
    | rev_stmt ';'    { $1 }
    | stmt            { [$1] }
    | {- empty -}   { [] }
rules : rev_rule { reverse $1 }
rev_rule
    : rev_rule ';' rule { $3 : $1 }
    | rev_rule ';'    { $1 }
    | rule            { [$1] }
    | {- empty -}   { [] }

var :: { Name }
    : uqvar { $1 }
    | LQVarId  {(parseName Val $1) }

uqvar :: { Name }
    : LVarId  { (toName Val $1) }
    | 'as'                  { vu_as }
    | 'family'              { vu_family }
    | 'hiding'              { vu_hiding }
    | 'qualified'           { vu_qualified }

    | 'alias'               { vu_alias }
    | 'kind'                { vu_kind }
    | 'closed'              { vu_closed }
--    | 'forall'              { u_forall }
--    | 'exists'              { u_exists }

gcon :: { Name }
    : '(' commas ')'   { tuple_con_name $2 }
    | con              { $1 }

con :: { Name }
    : LConId  { (toName DataConstructor $1) }
    | LQConId { (parseName DataConstructor $1) }
    | cons_id { quoteName dc_Cons }

varcon
    : var { $1 }
    | con { $1 }

varconop
    : varop { $1 }
    | conop { $1 }

conop :: { Name }
    : LConSym  { (toName DataConstructor $1) }
    | LQConSym { (parseName DataConstructor $1) }
--    | ':'     {% do implicitName dc_Cons }
    | cons_sym { quoteName dc_Cons }

varop :: { Name }
    : LVarSym  { (toName Val $1) }
    | LQVarSym { (parseName Val $1) }
    | '.'      { vu_Dot }
--    | '~'      { vu_Twiddle }
--    | '@'      { vu_At }

-- punctuation.


srcloc :: { SrcLoc } :       {%^ \ (L sl _ _) -> return sl }

{

happyError [] = do
    addWarn ParseError "parse error at EOF"
    parseNothing
happyError (L sl LLexError t:_) = do
    warn sl ParseError $ "lexer error " ++ show t
    parseNothing
happyError (L sl _ t:_) = do
    warn sl ParseError $ "parse error at " ++ show t
    parseNothing

x `cat` HsWords ws = HsWords (x:ws)
x `cat` y = HsWords [x,y]

eloc p e =  HsLocatedExp (Located (srcSpan p) e)
espan p1 p2 e =  HsLocatedExp (Located (SrcSpan p1 p2) e)
withSpan p1 p2 e =  withSrcSpan (SrcSpan p1 p2) e

tuple_con_name i = quoteName $ name_TupleConstructor termLevel (i + 1)

readPrim :: Read a => String -> a
readPrim s = case reads s of
    ~[(v,"#")] -> v

toKindVarSym n
    | Just k <- Map.lookup n kmap = return k
    | otherwise = parseErrorK $ "invalid kind: " ++ show n
    where kmap = Map.fromList
            [(vu_Star, hsKindStar)
            ,(vu_Hash, hsKindHash)
            ,(vu_Bang, hsKindBang)
            ,(vu_StarBang, hsKindStarBang)
            ,(vu_Quest, hsKindQuest)
            ,(vu_QuestQuest, hsKindQuestQuest)]

}
