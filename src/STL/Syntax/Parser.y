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

-- %name pModule    Module
-- %name pStatement Statement
%name pType      Type

%error     { parseError }
%tokentype { LocatedBy Position Token }
%monad     { Either String }

%token
  NL             { L _ TokNewline        }

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

  -- Keywords
  "forall"       { L _ (TokKeyword "forall") }
  "type"         { L _ (TokKeyword "type")   }
  "module"       { L _ (TokKeyword "module") }
  "import"       { L _ (TokKeyword "import") }
  "#eval"        { L _ (TokKeyword "#eval")  }
  "#check"       { L _ (TokKeyword "#check") }

  -- Base types
  "Unit"         { L _ (TokReserved "Unit") }
  "Void"         { L _ (TokReserved "Void") }
  "Integer"      { L _ (TokReserved "Integer") }
  "Double"       { L _ (TokReserved "Double") }
  "String"       { L _ (TokReserved "String") }
  "List"         { L _ (TokReserved "List") }
  "Dictionary"   { L _ (TokReserved "Dictionary") }
  "Natural"      { L _ (TokReserved "Natural") }
  "Array"        { L _ (TokReserved "Array") }

  -- Kinds
  "Type"         { L _ (TokReserved "Type") }
  "Row"          { L _ (TokReserved "Row") }
  "Presence"     { L _ (TokReserved "Presence") }
  "Nat"          { L _ (TokReserved "Nat") }

  CONSTRUCTOR    { L _ (TokConstructor _) }
  VARIABLE       { L _ (TokVariable    _) }

%left ':'
%right '->'

%%

-- Program

-- Module :: { Module }
--   : list(Statement)                      { $1 }

-- Statement

-- Statement :: { Statement }
--   : NL "import" ident                     { Load $ mkFileName $ getIdent $ extract $3 }
--   | NL ident '=' Type                     { Definition (Ident $ getIdent $ extract $2) $4 }
--   | NL "#check" Type                      { Check $3 }
--   | NL "#eval" Type                       { Eval $3 }

-- Type

Type :: { Type }
  : AppType                              { $1 }
  | AppType '->' sepBy1(Type,'->')       { mkArrow (position $2) ($1 : $3) }
  | "forall" list1(Bindings) '.' Type    { Fix $ TForall (position $1) (concat $2) $4 }

Bindings :: { [Binding] }
  : VARIABLE                             { [Binding (position $1) (Var $ getVariable $ extract $1) Nothing] }
  | '(' sepBy1(VARIABLE, ',')
    ':' Kind ')'                         { map (\idn -> Binding (position idn)
                                                    (Var $ getVariable $ extract idn)
                                                    (Just $4)) $2}

AppType :: { Type }
  : list1(AtomType)   { mkApplication $1 }

AtomType :: { Type }
  : BaseType          { Fix $ T (position $1) (extract $1) }
  | VARIABLE          { Fix $ TRef (position $1) (Var $ getVariable $ extract $1) }
  | CONSTRUCTOR       { Fix $ TGlobal (position $1) Nothing (GlobalName $ getConstructor $ extract $1) }
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
  | "Presence"    { Presence }
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

mkArrow :: Position -> [Type] -> Type
mkArrow pos (a : b : rest) = Fix $ TArrow pos a b rest
mkArrow pos _ = error "Unexpected input on mkArrow"

mkApplication :: [Type] -> Type
mkApplication (a : b : rest) = Fix $ TApp (getPosition a) a b rest
mkApplication (a : []) = a
mkApplication _ = error "Unexpected input on mkApplication"

mkFileName :: T.Text -> FilePath
mkFileName t = T.unpack t ++ ".stl"

type Located = LocatedBy Position

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "EOF: Unexpected end of file"
  (L pos tok : _) ->
    Left $ show $
      pretty pos <> colon <+> "Unexpected token:" <+> pretty tok

}
