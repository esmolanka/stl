{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax.Position where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..), colon)

data Position = Position
  { posFileName :: Text
  , posLine     :: {-# UNPACK #-} !Int
  , posColumn   :: {-# UNPACK #-} !Int
  } deriving (Show, Ord, Eq)

dummyPos :: Position
dummyPos = Position "<no location information>" 1 0

instance Pretty Position where
  pretty (Position fn lin col) =
    pretty fn <> colon <> pretty lin <> colon <> pretty col
