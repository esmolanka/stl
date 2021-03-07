{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module STL.Schema.Types
  ( Name(..)
  , FieldName(..)
  , CtorName(..)
  , VarName(..)
  , PrimType(..)
  , FieldOpt(..)
  , SchemaType(..)
  , SchemaDef(..)
  ) where

import Data.Text (Text)
import STL.Pretty

newtype Name = Name [Text] deriving (Show, Eq, Ord)
newtype FieldName = FieldName Text deriving (Show, Eq, Ord)
newtype CtorName = CtorName Text deriving (Show, Eq, Ord)
newtype VarName  = VarName Text deriving (Show, Eq, Ord)

data PrimType
  = PUnit
  | PVoid
  | PBool
  | PInt
  | PFloat
  | PString
  deriving (Show, Eq, Ord)

instance CPretty PrimType where
  cpretty = \case
    PUnit   -> aConstructor "Unit"
    PVoid   -> aConstructor "Void"
    PBool   -> aConstructor "Bool"
    PInt    -> aConstructor "Int"
    PFloat  -> aConstructor "Float"
    PString -> aConstructor "String"

data FieldOpt
  = RequiredField
  | OptionalField
  deriving (Show, Eq, Ord)

data SchemaType where
  SParam   :: VarName -> SchemaType
  SNamed   :: Name -> [SchemaType] -> SchemaType
  SPrim    :: PrimType -> SchemaType
  SArrow   :: [SchemaType] -> SchemaType -> SchemaType
  STuple   :: [SchemaType] -> SchemaType
  SArray   :: SchemaType -> SchemaType
  SRecord  :: [(FieldName, FieldOpt, SchemaType)] -> SchemaType
  SVariant :: [(CtorName, Maybe SchemaType)] -> SchemaType
  deriving (Show, Eq, Ord)

instance CPretty SchemaType where
  cpretty = ppSchemaType 0

ppSchemaType :: Int -> SchemaType -> Doc AnsiStyle
ppSchemaType lvl0 = pp lvl0
  where
    pp :: Int -> SchemaType -> Doc AnsiStyle
    pp lvl = \case
      SParam (VarName x) ->
        aVariable (pretty x)
      SNamed (Name xs) as ->
        aConstructor (hcat $ punctuate dot $ map pretty xs) <>
        if null as then mempty else parens (hsep $ punctuate comma $ map (pp 0) as)
      SPrim prim ->
        cpretty prim
      SArrow fs a ->
        let (open, close) = if lvl > 0 then (lparen, rparen) else (flatAlt space mempty, mempty)
        in ppList open close (flatAlt mempty space <> "→") (map (pp 1) fs ++ [pp 0 a])
      STuple ts ->
        ppList lparen rparen comma (map (pp 0) ts)
      SArray a ->
        brackets (pp 0 a)
      SRecord fs ->
        ppList lbrace rbrace comma (map ppField fs)
      SVariant cs ->
        ppList "⟨" "⟩" (flatAlt mempty space <> pipe) (map ppCtor cs)

    ppOpt :: FieldOpt -> Doc AnsiStyle
    ppOpt = \case
      RequiredField -> mempty
      OptionalField -> "?"

    ppField :: (FieldName, FieldOpt, SchemaType) -> Doc AnsiStyle
    ppField (FieldName lbl, opt, sty) = aLabel (pretty lbl) <> ppOpt opt <+> colon <+> pp 0 sty

    ppCtor :: (CtorName, Maybe SchemaType) -> Doc AnsiStyle
    ppCtor = \case
      (CtorName lbl, Nothing) -> aLabel (pretty lbl)
      (CtorName lbl, Just sty) -> aLabel (pretty lbl) <+> colon <+> pp 0 sty

    ppList :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
    ppList lb rb cm fields =
      group $ align $ enclose (lb <> flatAlt space mempty) (flatAlt line mempty <> rb) $ vcat $
        zipWith (<>) (mempty : repeat (cm <> space)) fields


data SchemaDef = SchemaDef
  { defName   :: Name
  , defParams :: [VarName]
  , defType   :: SchemaType
  } deriving (Show, Eq, Ord)

instance CPretty SchemaDef where
  cpretty (SchemaDef name params body) =
    let (Name nameParts) = name
        pName = hcat $ punctuate "." $ map pretty nameParts
        pParam (VarName x) = aVariable (pretty x)
        pParams =
          if null params
          then mempty
          else parens (hsep $ punctuate comma $ map pParam params)
    in group $ nest 2 $ vsep
       [ aKeyword "schema" <+> pName <> pParams <+> "="
       , cpretty body
       ]
