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

import Data.Functor.Foldable (Fix(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import Data.Text.Prettyprint.Doc

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
%monad     { Either String }

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
  "mutual"       { L _ (TokKeyword "mutual")   }
  "return"       { L _ (TokKeyword "return")   }
  "module"       { L _ (TokKeyword "module")   }
  "import"       { L _ (TokKeyword "import")   }
  "#eval"        { L _ (TokKeyword "#eval")    }
  "#check"       { L _ (TokKeyword "#check")   }

  CONSTRUCTOR    { L _ (TokConstructor _) }
  VARIABLE       { L _ (TokVariable    _) }

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
    ReturnType                { let (name, bindings) = $1
                                in Module name bindings $2 $3 $4 }

ModuleHeader :: { (ModuleName, [Binding]) }
  : {- empty -}               { ( ModuleName "Main", [] ) }
  | "module" CONSTRUCTOR
      list(Bindings)          { ( (ModuleName $ getConstructor $ extract $2)
                                , concat $3
                                ) }

ImportStatement :: { Import }
  : "import" CONSTRUCTOR      { Import (position $2)
                                  (ModuleName $ getConstructor $ extract $2)
                                  [] Nothing }
  | "import" CONSTRUCTOR '='
      CONSTRUCTOR
      list(AtomType)          { Import (position $2)
                                  (ModuleName $ getConstructor $ extract $4)
                                  $5 (Just (ModuleName $ getConstructor $ extract $2)) }

ReturnType :: { Maybe Type }
  : {- -}                     { Nothing }
  | "return" Type             { Just $2 }
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
  | "mutual" list(Bindings)
      list1(MutualClause)       { Mutualdef (position $1) (concat $2) $3  }
  | "#eval" Type                { Normalise (position $1 <> typePos $2) $2 }
  | "#check" Type '<:' Type     { Subsume (position $1 <> typePos $4) $2 $4 }

MutualClause :: { MutualClause }
  : '|' CONSTRUCTOR '=' Type    { MutualClause (position $2)
                                    (GlobalName $ getConstructor $ extract $2)
                                    $4 }

----------------------------------------------------------------------
-- Type

Type_ :: { Type }
  : Type EOF                             { $1 }

Type :: { Type }
  : AppType                              { $1 }
  | AppType '->' sepBy1(Type,'->')       { mkArrow ($1 : $3) }
  | "forall" list1(Bindings) '.' Type    { Fix $ TForall (position $1 <> typePos $4) (concat $2) $4 }
  | "exists" list1(Bindings) '.' Type    { Fix $ TExists (position $1 <> typePos $4) (concat $2) $4 }

Bindings :: { [Binding] }
  : VARIABLE                             { [Binding (position $1) (Var $ getVariable $ extract $1) Nothing] }
  | '(' sepBy1(VARIABLE, ',')
    ':' Kind ')'                         { map (\idn -> Binding (position idn)
                                                    (Var $ getVariable $ extract idn)
                                                    (Just $4)) $2}

AppType :: { Type }
  : list1(AtomType)   { mkApplication $1 }

AtomType :: { Type }
  : AtomType
    '[' AtomType ']'  { Fix $ TArray (typePos $1 <> position $4) $1 $3 }
  | VARIABLE          { Fix $ TRef (position $1) (Var $ getVariable $ extract $1) }
  | CONSTRUCTOR       { mkConstructor (position $1) (getConstructor $ extract $1) }
  | CONSTRUCTOR '.'
    CONSTRUCTOR       { Fix $ TGlobal (position $1)
                        (Just $ ModuleName $ getConstructor $ extract $3)
                        (GlobalName $ getConstructor $ extract $1) }
  | '(' Type ')'      { $2 }
  | '{' '}'                              { Fix $ TRecord (position $1 <> position $2) (RNil (position $2)) }
  | '{' RecordRow '}'                    { Fix $ TRecord (position $1 <> position $3) ($2 (position $3)) }
  | '<' '>'                              { Fix $ TVariant (position $1 <> position $2) (RNil (position $2)) }
  | '<' VariantRow '>'                   { Fix $ TVariant (position $1 <> position $3) ($2 (position $3)) }

RecordRow :: { Position -> Row Type }
  : sepBy1(RecRowExt, ',')               { \lastpos -> foldr ($) (RNil lastpos) $1 }
  | sepBy1(RecRowExt, ',') '|' AppType   { \_ -> foldr ($) (RExplicit (position $2) $3) $1 }
  | '|' AppType                          { \_ -> RExplicit (position $1) $2 }

RecRowExt :: { Row Type -> Row Type }
  : VARIABLE ':' Type                    { RExtend (position $1 <> typePos $3)
                                             (Label $ getVariable $ extract $1)
                                             (PPresent (position $1))
                                             $3 }
  | VARIABLE '?' ':' Type                { RExtend (position $1 <> typePos $4)
                                             (Label $ getVariable $ extract $1)
                                             (PVariable (position $3))
                                             $4 }
  | CONSTRUCTOR                          {% otherError (position $1) "record field names must start with a lower-case letter or an underscore" }

VariantRow :: { Position -> Row Type }
  : sepBy1(VarRowExt, ',')               { \lastpos -> foldr ($) (RNil lastpos) $1 }

VarRowExt :: { Row Type -> Row Type }
  : CONSTRUCTOR                          { RExtend (position $1)
                                             (Label $ getConstructor $ extract $1)
                                             (PVariable (position $1))
                                             (Fix (T (position $1) TUnit)) }
  | CONSTRUCTOR ':' Type                 { RExtend (position $1 <> typePos $3)
                                             (Label $ getConstructor $ extract $1)
                                             (PVariable (position $1))
                                             $3 }
  | VARIABLE                             {% otherError (position $1) "variant alternative names must start with an upper-case letter" }

----------------------------------------------------------------------
-- Kind

Kind :: { Kind }
  : AtomKind             { $1 }
  | AtomKind '->' Kind   { Arr $1 $3 }

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
mkArrow ts@(a : b : rest) = Fix $ TArrow (foldr1 (<>) $ map typePos ts) a b rest
mkArrow _ = error "Unexpected input on mkArrow"

mkApplication :: [Type] -> Type
mkApplication ts@(a : b : rest) = Fix $ TApp (foldr1 (<>) $ map typePos ts) a b rest
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
  "List"   -> Fix (T pos TList)
  "Dict"   -> Fix (T pos TDict)
  "Nat"    -> Fix (T pos TNat)
  other    -> Fix $ TGlobal pos Nothing (GlobalName ctor)

mkKind :: Position -> T.Text -> Either String Kind
mkKind pos ctor = case ctor of
  "Type" -> pure Star
  "Row"  -> pure Row
  "Nat"  -> pure Nat
  _other -> otherError pos ("unknown kind" <+> pretty ctor)

mkFileName :: T.Text -> FilePath
mkFileName t = T.unpack t ++ ".stl"

type Located = LocatedBy Position

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "Unexpected end of file"
  (L pos tok : _) ->
    Left $ show $ pretty pos <> colon <+> "error: unexpected" <+> pretty tok

otherError :: Position -> Doc a -> Either String b
otherError pos msg =
  Left $ show $ pretty pos <> colon <+> "error:" <+> msg

}
