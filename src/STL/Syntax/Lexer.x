{
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module STL.Syntax.Lexer
  ( lex
  ) where

import Prelude hiding (lex)
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read

import STL.Syntax.LexerInterface
import STL.Syntax.Token
import STL.Syntax.Position

}

$hspace     = [\ \t]
$whitespace = [$hspace\n\r\f\v]

$allgraphic = . # [\x00-\x20 \x7F-\xA0]

$digit      = 0-9
$alpha      = [a-z A-Z]
$lower      = [a-z]
$upper      = [A-Z]

@paren      = [\(\)\[\]\<\>\{\}]
@punct      = "=" | ":" | "->" | "|" | "." | ","
@keyword    = "forall" | "type" | "module" | "import" | "#eval" | "#check"
@reserved   = "Unit" | "Void" | "Integer" | "Double" | "String"
            | "List" | "Dictionary" | "Natural" | "Array"
            | "Type" | "Row" | "Presence" | "Nat"

$identrest  = [$alpha $digit _]
@constr     = [$upper] [$identrest]*
@variable   = [$lower _] [$identrest]*

:-

$whitespace+       ;
"--" .*            ;

@paren             { TokParen `via` BL.head }
@punct             { TokPunctuation `via` decode }
@keyword           { TokKeyword `via` decode }
@reserved          { TokReserved `via` decode }
@constr            { TokConstructor `via` decode }
@variable          { TokVariable `via` decode }

.                  { TokUnknown `via` BL.head    }

{
type AlexAction = LineCol -> ByteString -> LocatedBy LineCol Token

decode :: ByteString -> T.Text
decode = TL.toStrict . decodeUtf8

just :: Token -> AlexAction
just tok pos _ =
  L pos tok

via :: (a -> Token) -> (ByteString -> a) -> AlexAction
via ftok f pos str =
  (L pos) . ftok . f $ str

alexScanTokens :: AlexInput -> [LocatedBy LineCol Token]
alexScanTokens input =
  case alexScan input defaultCode of
    AlexEOF -> []
    AlexError (AlexInput {aiInput, aiLineCol = LineCol line col}) ->
      error $ "Lexical error at line " ++ show line ++ " column " ++ show col ++
        ". Remaining input: " ++ show (UTF8.take 200 aiInput)
    AlexSkip input _ -> alexScanTokens input
    AlexToken input' tokLen action ->
      if col == 1
        then L inputPosn TokNewline : tokens
        else tokens
      where
        tokens = action inputPosn inputText : alexScanTokens input'
        inputPosn@(LineCol _ col) = aiLineCol input
        inputText = UTF8.take (fromIntegral tokLen) (aiInput input)
  where
    defaultCode :: Int
    defaultCode = 0

lex :: Position -> ByteString -> [LocatedBy Position Token]
lex (Position fn line1 col1) =
  map (bimap fixPos id) . alexScanTokens . mkAlexInput (LineCol line1 col1)
  where
    fixPos (LineCol l c) = Position fn l c
}
