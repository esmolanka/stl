{-# LANGUAGE DefaultSignatures #-}

module STL.Pretty
  ( module Data.Text.Prettyprint.Doc
  , AnsiStyle, aKind, aKeyword, aConstructor, aVariable, aLabel
  , CPretty(..), putDocLn
  , ppSubscript
  , renderDoc
  ) where

import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)

import Data.Char
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text as ToText

class CPretty a where
  cpretty :: a -> Doc AnsiStyle

  default cpretty :: Pretty a => a -> Doc AnsiStyle
  cpretty = unAnnotate . pretty

aKind :: Doc AnsiStyle -> Doc AnsiStyle
aKind = annotate (colorDull Green)

aLabel :: Doc AnsiStyle -> Doc AnsiStyle
aLabel = annotate (color Red)

aKeyword :: Doc AnsiStyle -> Doc AnsiStyle
aKeyword = annotate (colorDull Blue <> bold)

aConstructor :: Doc AnsiStyle -> Doc AnsiStyle
aConstructor = annotate (colorDull Yellow)

aVariable :: Doc AnsiStyle -> Doc AnsiStyle
aVariable = annotate (colorDull Magenta)

ppSubscript :: Int -> Doc a
ppSubscript = pretty . mkSubscript . show
  where
    mkSubscript :: String -> String
    mkSubscript =
      map $ \c ->
        let code = ord c in
        if code >= 48 && code <= 57
        then chr (code - 48 + 8320)
        else c

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn doc = do
  supports <- hSupportsANSI stdout
  if supports
    then putDoc (doc <> line)
    else putDoc (unAnnotate (doc <> line))

renderDoc :: Doc a -> T.Text
renderDoc =
  ToText.renderStrict . layoutSmart defaultLayoutOptions

