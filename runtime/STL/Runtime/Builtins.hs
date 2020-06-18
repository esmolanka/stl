{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module STL.Runtime.Builtins
  ( (:~>) (..)
  ) where

import Data.Aeson

newtype a :~> b = ApiFunctionId Int deriving (Eq, Show)

infixr 3 :~>

instance FromJSON (a :~> b) where
  parseJSON = withObject "function" $ \v -> fmap ApiFunctionId (v .: "$")

instance ToJSON (a :~> b) where
  toJSON (ApiFunctionId n) = object ["$" .= n]
