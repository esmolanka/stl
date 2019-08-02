{-# LANGUAGE DefaultSignatures #-}

module STL.Pretty
  ( module Data.Text.Prettyprint.Doc
  , AnsiStyle, aKind, aKeyword, aConstructor, aVariable, aLabel
  , CPretty(..), putDocLn
  ) where

import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

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
aConstructor = annotate (color Blue)

aVariable :: Doc AnsiStyle -> Doc AnsiStyle
aVariable = annotate (colorDull Magenta)

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn doc = do
  supports <- hSupportsANSI stdout
  if supports
    then putDoc (doc <> line)
    else putDoc (unAnnotate (doc <> line))
