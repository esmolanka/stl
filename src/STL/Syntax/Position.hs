{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax.Position
  ( Position(..)
  , dummyPos
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..), colon)

data Position = Position
  { posFileName  :: Text
  , posLine      :: {-# UNPACK #-} !Int
  , posColumn    :: {-# UNPACK #-} !Int
  , posLineEnd   :: {-# UNPACK #-} !Int
  , posColumnEnd :: {-# UNPACK #-} !Int
  } deriving (Show, Ord, Eq)

instance Semigroup Position where
  Position fn l c _ _ <> Position _ _ _ l' c' =
    Position fn l c l' c'

dummyPos :: Position
dummyPos = Position "<no location information>" 1 1 1 1

instance Pretty Position where
  pretty (Position fn lin col lin' col') =
    pretty fn <> colon <>
      pretty lin <> colon <> pretty col <> "-" <>
      pretty lin' <> colon <> pretty col'
