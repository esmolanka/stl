{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Runtime (deriveSerialisation, (:~>)) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Language.Haskell.TH as TH

newtype a :~> b = MkFun Int
  deriving (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

deriveSerialisation :: Bool -> String -> TH.Name -> TH.Q [TH.Dec]
deriveSerialisation isSumType namePrefix =
  Aeson.deriveJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier      = drop (length namePrefix + 2)
    , Aeson.constructorTagModifier  = drop (length namePrefix + 1)
    , Aeson.allNullaryToStringTag   = False
    , Aeson.omitNothingFields       = True
    , Aeson.sumEncoding             = Aeson.ObjectWithSingleField
    , Aeson.unwrapUnaryRecords      = False
    , Aeson.tagSingleConstructors   = isSumType
    }
