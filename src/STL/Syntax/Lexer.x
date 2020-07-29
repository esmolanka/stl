{
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Char (isSpace)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
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

@punct      = "=" | ":" | "->" | "|" | "." | "," | "<:" | "?" | "-" | "+/-"

@keyword    = "forall" | "exists"
            | "type" | "with" | "provide"
            | "mix" | "mixin"
            | "record" $whitespace+ "of"
            | "variant" $whitespace+ "of"
            | "module" | "import"
            | "#eval" | "#check"

$identrest  = [$alpha $digit _]
@constr     = [$upper] [$identrest]*
@variable   = [$lower _] [$identrest]*
@quotedstr  = \' @variable \'

:-

$whitespace+       ;
"--" .*            ;

@paren             { TokParen `via` BL.head }
"⟨"                { just (TokParen '<') }
"⟩"                { just (TokParen '>') }
"→"                { just (TokPunctuation "->") }
"⊑"                { just (TokPunctuation "<:") }
"±"                { just (TokPunctuation "+/-") }
"×"                { just (TokPunctuation ",") }
@punct             { TokPunctuation `via` decode }
@keyword           { TokKeyword `via` decode }
"∀"                { just (TokKeyword "forall") }
"∃"                { just (TokKeyword "exists") }
@constr            { TokConstructor `via` decode }
@variable          { TokVariable `via` decode }
@quotedstr         { TokVariable `via` (decode . dropHeadLast) }

{
----------------------------------------------------------------------
-- Actions

just :: Token -> AlexAction
just tok _ = tok

via :: (a -> Token) -> (ByteString -> a) -> AlexAction
via ftok f = ftok . f

----------------------------------------------------------------------
-- Decoders

decode :: ByteString -> T.Text
decode = TL.toStrict . TL.unwords . TL.words . decodeUtf8With lenientDecode

dropHeadLast :: ByteString -> ByteString
dropHeadLast bs = BL.drop 1 (BL.take (BL.length bs - 1) bs)

----------------------------------------------------------------------
-- Entry point

lex :: Position -> ByteString -> [LocatedBy Position Token]
lex (Position fn line1 col1 _ _) =
  map (bimap fixPos id) . alexScanTokens . mkAlexInput (LineCol line1 col1)
  where
    fixPos (LineCol l c, len) = Position fn l c l (c + len)

----------------------------------------------------------------------
-- Machinery

type AlexAction = ByteString -> Token

alexScanTokens :: AlexInput -> [LocatedBy (LineCol, Int) Token]
alexScanTokens input =
  case alexScan input defaultCode of
    AlexEOF ->
      [L (aiLineCol input, 1) TokEOF]
    AlexError (AlexInput {aiInput, aiLineCol}) ->
      let rest = T.takeWhile (not . isSpace) $ decode $ UTF8.take 100 aiInput
      in [L (aiLineCol, T.length rest) (TokUnknown rest)]
    AlexSkip input _ ->
      alexScanTokens input
    AlexToken input' tokLen action ->
      let inputText = UTF8.take (fromIntegral tokLen) (aiInput input)
      in L (aiLineCol input, tokLen) (action inputText) : alexScanTokens input'
  where
    defaultCode :: Int
    defaultCode = 0

data LineCol = LineCol {-# UNPACK #-} !Int {-# UNPACK #-} !Int

columnsInTab :: Int
columnsInTab = 8

advanceLineCol :: Char -> LineCol -> LineCol
advanceLineCol '\n' (LineCol line _)   = LineCol (line + 1) 1
advanceLineCol '\t' (LineCol line col) = LineCol line (((col + columnsInTab - 1) `div` columnsInTab) * columnsInTab + 1)
advanceLineCol _    (LineCol line col) = LineCol line (col + 1)

data AlexInput = AlexInput
  { aiInput     :: ByteString
  , aiPrevChar  :: {-# UNPACK #-} !Char
  , aiBytesLeft :: {-# UNPACK #-} !Int64
  , aiLineCol   :: !LineCol
  }

mkAlexInput :: LineCol -> ByteString -> AlexInput
mkAlexInput initPos source = AlexInput
  { aiInput     = source
  , aiPrevChar  = '\n'
  , aiBytesLeft = 0
  , aiLineCol   = initPos
  }

alexNextChar :: AlexInput -> Maybe AlexInput
alexNextChar input
  | aiBytesLeft input > 1 = Just $ input { aiBytesLeft = aiBytesLeft input - 1 }
  | otherwise = case UTF8.decode (aiInput input) of
      Just (c, n) -> Just $ input
        { aiPrevChar  = c
        , aiLineCol   = advanceLineCol c (aiLineCol input)
        , aiBytesLeft = n
        }
      Nothing -> Nothing

-- Alex interface - functions used by Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
  alexNextChar input >>= getByte
  where
    getByte :: AlexInput -> Maybe (Word8, AlexInput)
    getByte input =
      case BLW.uncons (aiInput input) of
        Just (w, rest) -> Just (w, input { aiInput = rest })
        Nothing -> Nothing
}
