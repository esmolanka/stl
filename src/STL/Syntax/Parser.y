{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-tabs                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches      #-}

module STL.Syntax.Parser
  ( pType
  , pStatement
  , pModule
  ) where

import Data.Either
import Data.Functor.Foldable (Fix(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import Data.Text.Prettyprint.Doc
import Data.Void

import STL.Syntax.Lexer
import STL.Syntax.Position
import STL.Syntax.Token
import STL.Syntax.Types
}

%name pModule    Module_
%name pStatement Statement_
%name pType      Type_

%error     { parseError }
%tokentype { LocatedBy Position Token }
%monad     { Either (Position, Doc Void) }

%token
  '('            { L _ (TokParen '('   ) }
  ')'            { L _ (TokParen ')'   ) }
  '{'            { L _ (TokParen '{'   ) }
  '}'            { L _ (TokParen '}'   ) }
  '['            { L _ (TokParen '['   ) }
  ']'            { L _ (TokParen ']'   ) }
  '<'            { L _ (TokParen '<'   ) }
  '>'            { L _ (TokParen '>'   ) }

  '='            { L _ (TokPunctuation "="   ) }
  '-'            { L _ (TokPunctuation "-"   ) }
  '+/-'          { L _ (TokPunctuation "+/-" ) }
  ':'            { L _ (TokPunctuation ":"   ) }
  '->'           { L _ (TokPunctuation "->"  ) }
  '|'            { L _ (TokPunctuation "|"   ) }
  ','            { L _ (TokPunctuation ","   ) }
  '.'            { L _ (TokPunctuation "."   ) }
  '?'            { L _ (TokPunctuation "?"   ) }
  '<:'           { L _ (TokPunctuation "<:"  ) }

  -- Keywords
  "forall"       { L _ (TokKeyword "forall")   }
  "exists"       { L _ (TokKeyword "exists")   }
  "type"         { L _ (TokKeyword "type")     }
  "with"         { L _ (TokKeyword "with")     }
  "mix"          { L _ (TokKeyword "mix")      }
  "mixin"        { L _ (TokKeyword "mixin")    }
  "record of"    { L _ (TokKeyword "record of")  }
  "variant of"   { L _ (TokKeyword "variant of") }
  "provide"      { L _ (TokKeyword "provide")  }
  "module"       { L _ (TokKeyword "module")   }
  "import"       { L _ (TokKeyword "import")   }
  "#eval"        { L _ (TokKeyword "#eval")    }
  "#check"       { L _ (TokKeyword "#check")   }

  CONSTRUCTOR    { L _ (TokConstructor _) }
  VARIABLE       { L _ (TokVariable    _) }
  QUOTED         { L _ (TokQuotedLabel _) }

  EOF            { L _ TokEOF }

%left ':'
%right '->'

%%

----------------------------------------------------------------------
-- Module

Module_ :: { Module }
  : Module EOF                { $1 }

Module :: { Module }
  : ModuleHeader
    list(ImportStatement)
    list(Statement)
    ProvideType               {  Module $1 $2 $3 $4 }

ModuleHeader :: { [ModuleName] }
  : {- empty -}               { [ModuleName "Main"] }
  | "module"
    sepBy1(CONSTRUCTOR, '.')  { map (ModuleName . getConstructor . extract) $2 }

ImportStatement :: { Import }
  : "import"
    sepBy1(CONSTRUCTOR, '.')  { Import (foldl1 (<>) $ map position $2)
                                  (map (ModuleName . getConstructor . extract) $2)
                                  Nothing }
  | "import"
    sepBy1(CONSTRUCTOR, '.')
    VARIABLE {- as -}
    sepBy1(CONSTRUCTOR, '.')  {% case getVariable (extract $3) of
                                   "as" ->
                                     pure $ Import (foldl1 (<>) $ map position $2)
                                       (map (ModuleName . getConstructor . extract) $2)
                                       (Just (map (ModuleName . getConstructor . extract) $4))
                                   _ -> parseError [$3]
                              }

ProvideType :: { Maybe Type }
  : {- -}                     { Nothing }
  | "provide" Type            { Just $2 }
----------------------------------------------------------------------
-- Statement

Statement_ :: { Statement }
  : Statement EOF               { $1 }

Statement :: { Statement }
  : "type" CONSTRUCTOR
      list(Bindings) '='
      Type                      { Typedef (position $2)
                                   (GlobalName $ getConstructor $ extract $2)
                                   (concat $3)
                                   $5 }
  | "type" CONSTRUCTOR
      list(Bindings) '='
      Type
      list1(MutualClause)
                                { Mutualdef (position $2) (concat $3)
                                   (MutualClause
                                     (position $2)
                                     (GlobalName $ getConstructor $ extract $2)
                                     $5 : $6 ) }

  | "#eval" Type                { Normalise (position $1 <> typePos $2) $2 }
  | "#check" Type '<:' Type     { Subsume (position $1 <> typePos $4) $2 $4 }

MutualClause :: { MutualClause }
  : "with" CONSTRUCTOR '=' Type
                                { MutualClause (position $2)
                                    (GlobalName $ getConstructor $ extract $2)
                                    $4 }

----------------------------------------------------------------------
-- Type

Type_ :: { Type }
  : Type EOF                             { $1 }

Type :: { Type }
  : AppType                              { $1 }
  | AppType '->' sepBy1(AppType,'->')    { mkArrow ($1 : $3) }
  | "forall" list1(CovarBindings)
       '.' Type                          { Fix $ TForall (position $1 <> typePos $4) (concat $2) $4 }
  | "exists" list1(CovarBindings)
       '.' Type                          { Fix $ TExists (position $1 <> typePos $4) (concat $2) $4 }

  | "mixin" maybe(OneBinding)
    '|' sepBy1(VarMixin, '|')            { mkUnion $ mkMixins (position $1) $2 $4 }
  | "variant of" AtomType
    '|' sepBy1(VarMixin, '|')            { Fix $ TVariant (position $1) (mkUnion $ mkMixins (position $3) Nothing $4) (Just $2) }
  | '|' sepBy1(VarMixin, '|')            { Fix $ TVariant (position $1) (mkUnion $ mkMixins (position $1) Nothing $2) Nothing }

----------------------------------------------------------------------
-- Bindings

Variance :: { Variance }
  : {- empty -}                          { Covariant }
  | '-'                                  { Contravariant }
  | '+/-'                                { Invariant }

VarianceSpecific :: { Variance }
  : '-'                                  { Contravariant }
  | '+/-'                                { Invariant }

Bindings :: { [Binding Variance] }
  : Variance VARIABLE                    { [Binding (position $2) (Var $ getVariable $ extract $2) Nothing $1] }
  | '(' list1(both(Variance, VARIABLE))
    ':' Kind ')'                         { map (\ (variance, idn) -> Binding (position idn)
                                                    (Var $ getVariable $ extract idn)
                                                    (Just $4)
                                                    variance) $2}

CovarBindings :: { [Binding ()] }
  : VARIABLE                             { [Binding (position $1) (Var $ getVariable $ extract $1) Nothing ()] }
  | '(' list1(VARIABLE)
    ':' Kind ')'                         { map (\idn -> Binding (position idn)
                                                    (Var $ getVariable $ extract idn)
                                                    (Just $4)
                                                    ()) $2}

OneBinding :: { Binding () }
  : VARIABLE                             { Binding (position $1)  (Var $ getVariable $ extract $1) Nothing () }
  | '(' VARIABLE ':' Kind ')'            { Binding (position $2) (Var $ getVariable $ extract $2) (Just $4) () }

----------------------------------------------------------------------
-- Applications and special forms

AppType :: { Type }
  : list1(CompoundType)             { mkApplication $1 }

  | "record of" AtomType
    '{' '}'                         { Fix $ TRecord (position $1 <> position $4) (mkUnion $ mkMixins (position $3) Nothing []) (Just $2) }
  | "record of" AtomType
    '{' sepBy1(Mixin, ',') '}'      { Fix $ TRecord (position $1 <> position $5) (mkUnion $ mkMixins (position $3) Nothing $4) (Just $2) }

  | "variant of" AtomType
    '<' '>'                         { Fix $ TVariant (position $1 <> position $4) (mkUnion $ mkMixins (position $3) Nothing []) (Just $2) }
  | "variant of" AtomType
    '<' sepBy1(VarMixin, '|') '>'   { Fix $ TVariant (position $1 <> position $5) (mkUnion $ mkMixins (position $3) Nothing $4) (Just $2) }

  | "mixin" maybe(OneBinding)
    '{' '}'                         { mkUnion $ mkMixins (position $3) $2 [] }
  | "mixin" maybe(OneBinding)
    '{' sepBy1(Mixin, ',') '}'      { mkUnion $ mkMixins (position $3) $2 $4 }

  | "mixin" maybe(OneBinding)
    '<' '>'                         { mkUnion $ mkMixins (position $3) $2 [] }
  | "mixin" maybe(OneBinding)
    '<' sepBy1(VarMixin, '|') '>'   { mkUnion $ mkMixins (position $3) $2 $4 }

----------------------------------------------------------------------
-- Compound types

CompoundType :: { Type }
  : AtomType                        { $1 }
  | CompoundType '[' AtomType ']'   { Fix $ TArray (typePos $1 <> position $4) $1 $3 }
  | '(' AppType ',' ')'             { mkTuple [$2] }
  | '(' sepBy2(AppType, ',') ')'    { mkTuple $2 }

  | '{' '}'                         { Fix $ TRecord (position $1 <> position $2) (mkUnion $ mkMixins (position $1) Nothing []) Nothing }
  | '{' sepBy1(Mixin, ',') '}'      { Fix $ TRecord (position $1 <> position $3) (mkUnion $ mkMixins (position $1) Nothing $2) Nothing }

  | '<' '>'                         { Fix $ TVariant (position $1 <> position $2) (mkUnion $ mkMixins (position $1) Nothing []) Nothing }
  | '<' sepBy1(VarMixin, '|') '>'   { Fix $ TVariant (position $1 <> position $3) (mkUnion $ mkMixins (position $1) Nothing $2) Nothing }

----------------------------------------------------------------------
-- Atomic types

AtomType :: { Type }
  : VARIABLE           { Fix $ TRef (position $1) (Var $ getVariable $ extract $1) }
  | CONSTRUCTOR        { mkConstructor (position $1) (getConstructor $ extract $1) }

  | CONSTRUCTOR '.'
    CONSTRUCTOR        { Fix $ TGlobal (position $1)
                         (Just $ ModuleName $ getConstructor $ extract $3)
                         (GlobalName $ getConstructor $ extract $1) }
  | '(' Type ')'       { $2 }

----------------------------------------------------------------------
-- Records, Variants, and Mixins

Mixin :: { Either (Row Type -> Row Type) Type }
  : RecRowExt          { Left  $1 }
  | "mix" Type         { Right $2 }

VarMixin :: { Either (Row Type -> Row Type) Type }
  : VarRowExt          { Left  $1 }
  | "mix" Type         { Right $2 }

RecRowExt :: { Row Type -> Row Type }
  : LowerOrQuotedLabel ':' Type          { RExtend (position $1 <> typePos $3)
                                             (extract $1)
                                             (PPresent (position $1))
                                             $3 }
  | LowerOrQuotedLabel '?' ':' Type      { RExtend (position $1 <> typePos $4)
                                             (extract $1)
                                             (PVariable (position $3))
                                             $4 }
  | CONSTRUCTOR ':'                      {% otherError (position $1) "record field names must start with a lower-case letter or an underscore" }

VarRowExt :: { Row Type -> Row Type }
  : UpperOrQuotedLabel                   { RExtend (position $1)
                                             (extract $1)
                                             (PVariable (position $1))
                                             (Fix (T (position $1) TUnit)) }
  | UpperOrQuotedLabel ':' Type          { RExtend (position $1 <> typePos $3)
                                             (extract $1)
                                             (PVariable (position $1))
                                             $3 }
  | VARIABLE                             {% otherError (position $1) "variant alternative names must start with an upper-case letter" }


LowerOrQuotedLabel :: { Located Label }
  : VARIABLE           { (Label $ getVariable $ extract $1) @@ $1 }
  | QUOTED             { (Label $ getQuotedLabel $ extract $1) @@ $1 }

UpperOrQuotedLabel :: { Located Label }
  : CONSTRUCTOR        { (Label $ getConstructor $ extract $1) @@ $1 }
  | QUOTED             { (Label $ getQuotedLabel $ extract $1) @@ $1 }

----------------------------------------------------------------------
-- Kind

Kind :: { Kind }
  : AtomKind             { $1 }
  | AtomKind '->' Kind   { Arr $1 Covariant $3 }
  | VarianceSpecific
      AtomKind '->' Kind { Arr $2 $1 $4 }

AtomKind :: { Kind }
  : CONSTRUCTOR   {% mkKind (position $1) (getConstructor $ extract $1) }
  | '(' Kind ')'  { $2 }

----------------------------------------------------------------------
-- Utils

rev_list1(p)
  : p                      { [$1]    }
  | rev_list1(p) p         { $2 : $1 }

list1(p)
  : rev_list1(p)           { reverse $1 }

list(p)
  : {- empty -}            { [] }
  | list1(p)               { $1 }

maybe(p)
  : {- -}                  { Nothing }
  | p                      { Just $1 }

fst(p, q)
  : p q                    { $1 }

snd(p, q)
  : p q                    { $2 }

both(p, q)
  : p q                    { ($1, $2) }

sepBy1(p, q)
  : p list(snd(q, p))      { $1 : $2 }

sepBy2(p, q)
  : p q sepBy1(p, q)       { $1 : $3 }

{
--

mkArrow :: [Type] -> Type
mkArrow ts@(a : b : rest) = Fix $ TArrow (foldMap typePos ts) a b rest
mkArrow _ = error "Unexpected input on mkArrow"

mkTuple :: [Type] -> Type
mkTuple ts@(a : rest) = Fix $ TTuple (foldMap typePos ts) a rest
mkTuple _ = error "Unexpected input on mkTuple"

mkUnion :: [Type] -> Type
mkUnion [] = Fix $ TMixin dummyPos Nothing (RNil dummyPos)
mkUnion (a : []) = a
mkUnion ts@(a : b : rest) = Fix $ TUnion (foldMap typePos ts) a b rest

mkMixins :: Position -> Maybe (Binding ()) -> [Either (Row Type -> Row Type) Type] -> [Type]
mkMixins pos bnd ts =
  let (rowexts, mixins) = partitionEithers ts
      row :: Type
      row = Fix $ TMixin pos bnd $ foldr ($) (RNil pos) rowexts
  in if null rowexts then mixins else row : mixins

mkApplication :: [Type] -> Type
mkApplication ts@(a : b : rest) = Fix $ TApp (foldMap typePos ts) a b rest
mkApplication (a : []) = a
mkApplication _ = error "Unexpected input on mkApplication"

mkConstructor :: Position -> T.Text -> Type
mkConstructor pos ctor = case ctor of
  "Unit"   -> Fix (T pos TUnit)
  "Void"   -> Fix (T pos TVoid)
  "Bool"   -> Fix (T pos TBool)
  "Int"    -> Fix (T pos TInt)
  "Float"  -> Fix (T pos TFloat)
  "String" -> Fix (T pos TString)
  "Dict"   -> Fix (T pos TDict)
  "Nat"    -> Fix (T pos TNat)
  other    -> Fix $ TGlobal pos Nothing (GlobalName ctor)

mkKind :: Position -> T.Text -> Either (Position, Doc Void) Kind
mkKind pos ctor = case ctor of
  "Type" -> pure Star
  "Row"  -> pure Row
  "Nat"  -> pure Nat
  _other -> otherError pos ("unknown kind" <+> pretty ctor)

mkFileName :: T.Text -> FilePath
mkFileName t = T.unpack t ++ ".types"

type Located = LocatedBy Position

parseError :: [Located Token] -> Either (Position, Doc Void) b
parseError toks = case toks of
  (L pos tok : _) ->
    Left (pos, "unexpected" <+> pretty tok)
  [] ->
    -- Well-formed token streams must end with EOF token
    error "Unexpected end of token stream"

otherError :: Position -> Doc Void -> Either (Position, Doc Void) b
otherError pos msg = Left (pos, msg)
}
