{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax
  ( module STL.Syntax.Types
  , module STL.Syntax.Position
  , parseType
  , parseStatement
  , parseModule
  , parseType'
  , parseStatement'
  , parseModule'
  ) where

import Control.Arrow (left, second)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Void

import STL.Syntax.Types
import STL.Syntax.Position
import qualified STL.Syntax.Parser as Parser
import qualified STL.Syntax.Lexer as Lexer

parseType' :: FilePath -> BL.ByteString -> Either (Position, Doc ann) Type
parseType' fn = left (second unAnnotate) . Parser.pType . Lexer.lex (dummyPos { posFileName = pack fn })

parseStatement' :: FilePath -> BL.ByteString -> Either (Position, Doc ann) Statement
parseStatement' fn = left (second unAnnotate) . Parser.pStatement . Lexer.lex (dummyPos { posFileName = pack fn })

parseModule' :: FilePath -> BL.ByteString -> Either (Position, Doc ann) Module
parseModule' fn = left (second unAnnotate) . Parser.pModule . Lexer.lex (dummyPos { posFileName = pack fn })

showParseError :: (Position, Doc Void) -> String
showParseError (pos, msg) =
  show $ pretty pos <> colon <+> "error:" <+> msg

parseType :: FilePath -> BL.ByteString -> Either String Type
parseType fn str = left showParseError $ parseType' fn str

parseStatement :: FilePath -> BL.ByteString -> Either String Statement
parseStatement fn str = left showParseError $ parseStatement' fn str

parseModule :: FilePath -> BL.ByteString -> Either String Module
parseModule fn str = left showParseError $ parseModule' fn str
