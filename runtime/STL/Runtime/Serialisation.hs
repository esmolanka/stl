{-# LANGUAGE ConstraintKinds #-}

module STL.Runtime.Serialisation
  ( deriveSerialisation
  , Serialisable
  ) where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

type Serialisable a =
  ( FromJSON a
  , ToJSON a
  )

deriveSerialisation :: Bool -> String -> Name -> Q [Dec]
deriveSerialisation isSumType prefix =
  deriveJSON (defaultOptions
    { fieldLabelModifier = id
    , constructorTagModifier = drop (length prefix + 1)
    , allNullaryToStringTag = False
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = False
    , tagSingleConstructors = isSumType
    })
