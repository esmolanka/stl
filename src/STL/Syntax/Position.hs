{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax.Position
  ( Position(..)
  , dummyPos
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Pretty (..), colon)

data Position = Position
  { posFileName  :: Text
  , posLine      :: {-# UNPACK #-} !Int
  , posColumn    :: {-# UNPACK #-} !Int
  , posLineEnd   :: {-# UNPACK #-} !Int
  , posColumnEnd :: {-# UNPACK #-} !Int
  } deriving (Show, Ord, Eq)

instance Semigroup Position where
  p@(Position fn l c _ _) <> q@(Position fn' _ _ l' c')
    | "<" <- T.take 1 fn = q -- '<no location information>' case
    | fn == fn' = Position fn l c l' c'
    | otherwise = p

instance Monoid Position where
  mempty = dummyPos

dummyPos :: Position
dummyPos = Position "<no location information>" 1 1 1 1

instance Pretty Position where
  pretty (Position fn lin col lin' col') =
    pretty fn <> colon <>
      pretty lin <> colon <> pretty col <> "-" <>
      pretty lin' <> colon <> pretty col'
