{
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module STL.Syntax.Lexer (lex) where

import Prelude hiding (lex)

import Data.Bifunctor
import qualified Data.ByteString.Lazy as BLW
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read
import Data.Word

import STL.Syntax.Position
import STL.Syntax.Token

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
----------------------------------------------------------------------
-- Actions

just :: Token -> AlexAction
just tok pos _ =
  L pos tok

via :: (a -> Token) -> (ByteString -> a) -> AlexAction
via ftok f pos str =
  (L pos) . ftok . f $ str

----------------------------------------------------------------------
-- Decoders

decode :: ByteString -> T.Text
decode = TL.toStrict . decodeUtf8

----------------------------------------------------------------------
-- Entry point

lex :: Position -> ByteString -> [LocatedBy Position Token]
lex (Position fn line1 col1) =
  map (bimap fixPos id) . alexScanTokens . mkAlexInput (LineCol line1 col1)
  where
    fixPos (LineCol l c) = Position fn l c

----------------------------------------------------------------------
-- Machinery

type AlexAction = LineCol -> ByteString -> LocatedBy LineCol Token

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

data LineCol = LineCol {-# UNPACK #-} !Int {-# UNPACK #-} !Int

columnsInTab :: Int
columnsInTab = 8

advanceLineCol :: Char -> LineCol -> LineCol
advanceLineCol '\n' (LineCol line _)   = LineCol (line + 1) 0
advanceLineCol '\t' (LineCol line col) = LineCol line (((col + columnsInTab - 1) `div` columnsInTab) * columnsInTab + 1)
advanceLineCol _    (LineCol line col) = LineCol line (col + 1)

data AlexInput = AlexInput
  { aiInput     :: ByteString
  , aiPrevChar  :: {-# UNPACK #-} !Char
  , aiCurChar   :: {-# UNPACK #-} !Char
  , aiBytesLeft :: {-# UNPACK #-} !Int64
  , aiLineCol   :: !LineCol
  }

mkAlexInput :: LineCol -> ByteString -> AlexInput
mkAlexInput initPos source = alexNextChar $ AlexInput
  { aiInput     = source
  , aiPrevChar  = '\n'
  , aiCurChar   = '\n'
  , aiBytesLeft = 0
  , aiLineCol   = initPos
  }

alexNextChar :: AlexInput -> AlexInput
alexNextChar input =
  case UTF8.decode (aiInput input) of
    Just (c, n) -> input
      { aiPrevChar  = aiCurChar input
      , aiCurChar   = c
      , aiBytesLeft = n
      }
    Nothing     -> input
      { aiPrevChar  = aiCurChar input
      , aiCurChar   = '\n'
      , aiBytesLeft = 0
      }

alexPropagatePos :: AlexInput -> AlexInput
alexPropagatePos input =
  input { aiLineCol = advanceLineCol (aiPrevChar input) (aiLineCol input) }

-- Alex interface - functions usedby Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input
  | aiBytesLeft input == 0 = go . alexPropagatePos . alexNextChar $ input
  | otherwise = go input
  where
    go :: AlexInput -> Maybe (Word8, AlexInput)
    go input =
      case BLW.uncons (aiInput input) of
        Just (w, rest) -> Just (w, input
          { aiBytesLeft = aiBytesLeft input - 1
          , aiInput     = rest
          })
        Nothing -> Nothing
}
