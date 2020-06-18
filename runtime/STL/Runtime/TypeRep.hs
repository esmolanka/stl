{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module STL.Runtime.TypeRep
  ( module Data.StructuralType
  , TypeOf(..)
  , mkLabel
  , isSubsumedBy
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Either
import qualified Data.Text as T

import qualified STL.Core.Subsumption as Core

import Data.StructuralType

class TypeOf a where
  typeOf :: forall proxy. proxy a -> SType

mkLabel :: String -> Label
mkLabel = Label . T.pack

isSubsumedBy :: SType -> SType -> Bool
isSubsumedBy a b =
  isRight . fst $
    Core.runSubsumption
      mempty
      (inject a `Core.subsumedBy` inject b)

deriving instance FromJSON Label
deriving instance ToJSON Label

deriveJSON
  (defaultOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = False
    , tagSingleConstructors = True
    }) ''BaseType

deriveJSON
  (defaultOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = False
    , tagSingleConstructors = True
    }) ''Kind

deriveJSON
  (defaultOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = False
    , tagSingleConstructors = True
    }) ''Variance

deriveJSON
  (defaultOptions
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = False
    , tagSingleConstructors = True
    }) ''SType
