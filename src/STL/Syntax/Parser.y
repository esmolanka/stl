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
  '+'            { L _ (TokPunctuation "+"   ) }
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

  -- Base types
  "Unit"         { L _ (TokReserved "Unit")       }
  "Void"         { L _ (TokReserved "Void")       }
  "Integer"      { L _ (TokReserved "Integer")    }
  "Double"       { L _ (TokReserved "Double")     }
  "String"       { L _ (TokReserved "String")     }
  "List"         { L _ (TokReserved "List")       }
  "Dictionary"   { L _ (TokReserved "Dictionary") }
  "Natural"      { L _ (TokReserved "Natural")    }
  "Array"        { L _ (TokReserved "Array")      }

  -- Kinds
  "Type"         { L _ (TokReserved "Type")     }
  "Row"          { L _ (TokReserved "Row")      }
  "Nat"          { L _ (TokReserved "Nat")      }

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

AppType :: { Type }
  : list1(AtomType)   { mkApplication $1 }
  | '{' '}'                              { Fix $ TRecord (position $1 <> position $2) (RNil (position $2)) }
  | '{' RecordRow '}'                    { Fix $ TRecord (position $1 <> position $3) ($2 (position $3)) }
  | '<' '>'                              { Fix $ TVariant (position $1 <> position $2) (RNil (position $2)) }
  | '<' VariantRow '>'                   { Fix $ TVariant (position $1 <> position $3) ($2 (position $3)) }

Bindings :: { [Binding] }
  : VARIABLE                             { [Binding (position $1) (Var $ getVariable $ extract $1) Nothing] }
  | '(' sepBy1(VARIABLE, ',')
    ':' Kind ')'                         { map (\idn -> Binding (position idn)
                                                    (Var $ getVariable $ extract idn)
                                                    (Just $4)) $2}

RecordRow :: { Position -> Row Type }
  : sepBy1(RecRowExt, ',')               { \lastpos -> foldr ($) (RNil lastpos) $1 }
  | sepBy1(RecRowExt, ',') '|' AppType   { \_ -> foldr ($) (RExplicit (position $2) $3) $1 }
  | '|' AppType                          { \_ -> RExplicit (position $1) $2 }

RecRowExt :: { Row Type -> Row Type }
  : VARIABLE ':' Type                    { RExtend (position $1 <> typePos $3)
                                             (Label $ getVariable $ extract $1)
                                             (PVariable (position $1))
                                             $3 }
  | VARIABLE '+' ':' Type                { RExtend (position $1 <> typePos $4)
                                             (Label $ getVariable $ extract $1)
                                             (PPresent (position $3))
                                             $4 }
  | CONSTRUCTOR                          {% otherError (position $1) "labels in records must start with a lower-case letter or an underscore" }

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
  | VARIABLE                             {% otherError (position $1) "labels in variants must start with an upper-case letter" }

AtomType :: { Type }
  : BaseType          { Fix $ T (position $1) (extract $1) }
  | VARIABLE          { Fix $ TRef (position $1) (Var $ getVariable $ extract $1) }
  | CONSTRUCTOR       { Fix $ TGlobal (position $1) Nothing (GlobalName $ getConstructor $ extract $1) }
  | CONSTRUCTOR '.'
    CONSTRUCTOR       { Fix $ TGlobal (position $1)
                        (Just $ ModuleName $ getConstructor $ extract $3)
                        (GlobalName $ getConstructor $ extract $1) }
  | '(' Type ')'      { $2 }

BaseType :: { Located BaseType }
  : "Unit"            { TUnit       @@ $1 }
  | "Void"            { TVoid       @@ $1 }
  | "Integer"         { TInteger    @@ $1 }
  | "Double"          { TDouble     @@ $1 }
  | "String"          { TString     @@ $1 }
  | "List"            { TList       @@ $1 }
  | "Dictionary"      { TDictionary @@ $1 }
  | "Natural"         { TNatural    @@ $1 }
  | "Array"           { TArray      @@ $1 }

----------------------------------------------------------------------
-- Kind

Kind :: { Kind }
  : AtomKind             { $1 }
  | AtomKind '->' Kind   { Arr $1 $3 }

AtomKind :: { Kind }
  : "Type"        { Star }
  | "Row"         { Row }
  | "Nat"         { Nat }
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

mkFileName :: T.Text -> FilePath
mkFileName t = T.unpack t ++ ".stl"

type Located = LocatedBy Position

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "Unexpected end of file"
  (L pos tok : _) ->
    Left $ show $ pretty pos <> colon <+> "error: unexpected" <+> pretty tok

otherError :: Position -> String -> Either String b
otherError pos msg =
  Left $ show $ pretty pos <> colon <+> "error:" <+> pretty msg

}
