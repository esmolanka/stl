{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Runtime where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

newtype a :~> b = MkFun Int
  deriving (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

jsonOptions :: Bool -> Int -> Aeson.Options
jsonOptions isADT n = Aeson.defaultOptions
  { Aeson.fieldLabelModifier      = drop (n + 2)
  , Aeson.constructorTagModifier  = drop (n + 1)
  , Aeson.allNullaryToStringTag   = False
  , Aeson.omitNothingFields       = True
  , Aeson.sumEncoding             = Aeson.ObjectWithSingleField
  , Aeson.unwrapUnaryRecords      = False
  , Aeson.tagSingleConstructors   = isADT
  }
