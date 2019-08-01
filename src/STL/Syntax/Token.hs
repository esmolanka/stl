{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax.Token
  ( Token (..)
  , LocatedBy (..)
  , mapPosition
  , extract
  , position
  , (@@)
  ) where

import Data.Bifunctor
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data Token
  = TokNewline                                    -- indicator of a new line
  | TokParen       { getParen        :: !Char }   -- [, ], (, ), {, }, <, >
  | TokPunctuation { getPunctuation  :: !Text }   -- ':', '->', '|', ',', '.'
  | TokKeyword     { getKeyword      :: !Text }   -- forall, type, mutual
  | TokReserved    { getReserved     :: !Text }   -- Unit, String, List
  | TokConstructor { getConstructor  :: !Text }   -- Maybe, Just, Nothing
  | TokVariable    { getVariable     :: !Text }   -- a, b, head, tail
  | TokUnknown     { getUnknown      :: !Char }   -- for unknown lexemes
    deriving (Show, Eq)

instance Pretty Token where
  pretty = \case
    TokNewline       -> "newline"
    TokParen x       -> "paren" <+> pretty  (show x)
    TokPunctuation x -> "punctuation" <+> parens  (pretty x)
    TokKeyword x     -> "keyword" <+> squotes (pretty x)
    TokReserved x    -> "reserved identifier" <+> squotes (pretty x)
    TokConstructor x -> "identifier" <+> squotes (pretty x)
    TokVariable x    -> "identifier" <+> squotes (pretty x)
    TokUnknown x     -> "unknown" <+> pretty  (show x)

data LocatedBy p a = L !p !a
  deriving (Show, Eq, Functor)

instance Bifunctor LocatedBy where
  bimap f g (L a b) = L (f a) (g b)

{-# INLINE mapPosition #-}
mapPosition :: (p -> p') -> LocatedBy p a -> LocatedBy p' a
mapPosition f (L p a) = L (f p) a

extract :: LocatedBy p a -> a
extract (L _ a) = a

position :: LocatedBy p a -> p
position (L p _) = p

infix 9 @@

(@@) :: a -> LocatedBy p b -> LocatedBy p a
(@@) a (L p _) = (L p a)