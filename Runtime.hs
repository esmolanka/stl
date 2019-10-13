{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Runtime
  ( deriveSerialisation
  , (:~>)
  , module Data.StructuralType
  , TypeOf(..)
  , mkLabel
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Data.StructuralType

class TypeOf a where
  typeOf :: proxy a -> SType

mkLabel :: String -> Label
mkLabel = Label . T.pack

newtype a :~> b = MkFun Int
  deriving (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

deriveSerialisation :: Bool -> String -> TH.Name -> TH.Q [TH.Dec]
deriveSerialisation isSumType namePrefix =
  Aeson.deriveJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier      = id
    , Aeson.constructorTagModifier  = drop (length namePrefix + 1)
    , Aeson.allNullaryToStringTag   = False
    , Aeson.omitNothingFields       = True
    , Aeson.sumEncoding             = Aeson.ObjectWithSingleField
    , Aeson.unwrapUnaryRecords      = False
    , Aeson.tagSingleConstructors   = isSumType
    }
