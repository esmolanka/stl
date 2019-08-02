module STL.Syntax
  ( module STL.Syntax.Types
  , module STL.Syntax.Position
  , parseType
  , parseStatement
  , parseModule
  ) where

import Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as BL

import STL.Syntax.Types
import STL.Syntax.Position
import qualified STL.Syntax.Parser as Parser
import qualified STL.Syntax.Lexer as Lexer

parseType :: FilePath -> BL.ByteString -> Either String Type
parseType fn = Parser.pType . Lexer.lex (dummyPos { posFileName = pack fn })

parseStatement :: FilePath -> BL.ByteString -> Either String Statement
parseStatement fn = Parser.pStatement . Lexer.lex (dummyPos { posFileName = pack fn })

parseModule :: FilePath -> BL.ByteString -> Either String Module
parseModule fn = Parser.pModule . Lexer.lex (dummyPos { posFileName = pack fn })
