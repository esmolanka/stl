{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wwarn #-}

module STL.CodeGen.GenHaskell (genHaskell) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)

import STL.Pretty
import qualified STL.Schema as S
import Data.StructuralType

----------------------------------------------------------------------
-- Target types

newtype Name = Name [Text] deriving (Show, Eq, Ord)
newtype FieldName = FieldName Text deriving (Show, Eq, Ord)
newtype CtorName = CtorName Text deriving (Show, Eq, Ord)
newtype VarName  = VarName Text deriving (Show, Eq, Ord)

data HaskellType
  = HGlobal Name
  | HRef VarName
  | HArrow HaskellType HaskellType
  | HApp HaskellType HaskellType
  | HTuple [HaskellType]
  | HUnit
  | HVoid
  | HBool
  | HInt
  | HFloat
  | HString
  | HMaybe
  | HList
  | HDict
  | HNat
  deriving (Show, Eq, Ord)

spine :: HaskellType -> (HaskellType, [HaskellType])
spine t = let (f, as) = go t in (f, reverse as)
  where
    go = \case
      HApp a c -> let (f, b) = go a in (f, c : b)
      other -> (other, [])

instance CPretty HaskellType where
  cpretty = ppHaskellType

data HaskellDef
  = Newtype
    { hdefName :: Name
    , hdefParams :: [VarName]
    , hdefType :: HaskellType
    }
  | Record
    { hdefName :: Name
    , hdefParams :: [VarName]
    , hdefFields :: [(FieldName, HaskellType)]
    }
  | SumType
    { hdefName :: Name
    , hdefParams :: [VarName]
    , hdefCases :: [(CtorName, Maybe (Either [(FieldName, HaskellType)] HaskellType))]
    }
  deriving (Show, Eq, Ord)

instance CPretty HaskellDef where
  cpretty = ppHaskellDef

----------------------------------------------------------------------
-- Generation monad

type MonadGen m =
  ( MonadReader GenEnv m
  , MonadState GenState m
  , MonadError (Doc AnsiStyle) m
  )

data GenState = GenState
  { sRegistered    :: Map Name HaskellDef
  , sRegisteredOrd :: [Name]
  , sNameMapping   :: Map S.Name Name
  }

data GenEnv = GenEnv
  { eDerivedName :: Name
  , eParameters  :: [VarName]
  }

register :: (MonadGen m) => HaskellDef -> m ()
register def =
  modify $ \s ->
    s { sRegistered = M.insert (hdefName def) def (sRegistered s)
      , sRegisteredOrd = hdefName def : sRegisteredOrd s
      }

refreshName :: (MonadGen m) => Name -> m Name
refreshName (Name n) = do
  used1 <- gets (M.keysSet . sRegistered)
  used2 <- gets (S.fromList . M.elems . sNameMapping)
  let used = S.union used1 used2
  pure $ head $ filter (not . flip S.member used) (map Name $ iterate (\m -> "'" : m) n)

withSuffix :: MonadGen m => Text -> m a -> m a
withSuffix suffix =
  local (\s -> s { eDerivedName = addSuffix (eDerivedName s) })
  where
    addSuffix (Name parts) = Name (suffix : parts)

forDefinition :: MonadGen m => Name -> [VarName] -> m a -> m a
forDefinition name params =
  local (\s -> s { eDerivedName = name, eParameters = params })

runGen :: ExceptT (Doc AnsiStyle) (ReaderT GenEnv (State GenState)) a -> Either (Doc AnsiStyle) a
runGen a =
  evalState
    (runReaderT (runExceptT a) (GenEnv (Name ["API"]) []))
    (GenState M.empty [] M.empty)

----------------------------------------------------------------------

genName :: (MonadGen m) => S.Name -> m Name
genName global@(S.Name name) = do
  mapping <- gets sNameMapping
  case M.lookup global mapping of
    Just known -> pure known
    Nothing    -> do
      new <- refreshName $ Name name
      modify (\s -> s { sNameMapping = M.insert global new (sNameMapping s) })
      pure new

genBaseType :: MonadGen m => S.PrimType -> m HaskellType
genBaseType = \case
  S.PUnit   -> pure HUnit
  S.PVoid   -> pure HVoid
  S.PBool   -> pure HBool
  S.PInt    -> pure HInt
  S.PFloat  -> pure HFloat
  S.PString -> pure HString

data DelayedType
  = MkSumType [(CtorName, Maybe (Either [(FieldName, HaskellType)] HaskellType))]
  | MkRecord  [(FieldName, HaskellType)]
  | Inline HaskellType

applyParams :: HaskellType -> [VarName] -> HaskellType
applyParams = foldl (\a v -> HApp a (HRef v))

fromDelayed :: MonadGen m => DelayedType -> m HaskellType
fromDelayed = \case
  MkSumType ctors -> do
    name <- refreshName =<< asks eDerivedName
    params <- asks eParameters
    register $ SumType name params ctors
    pure $ applyParams (HGlobal name) params
  MkRecord fields -> do
    name <- refreshName =<< asks eDerivedName
    params <- asks eParameters
    register $ Record name params fields
    pure $ applyParams (HGlobal name) params
  Inline t -> pure t

genDefBody :: forall m. MonadGen m => S.SchemaType -> m HaskellDef
genDefBody ty0 = do
  delayed <- go ty0
  defName <- asks eDerivedName
  params  <- asks eParameters
  case delayed of
    MkSumType ctors -> pure $ SumType defName params ctors
    MkRecord fields -> pure $ Record defName params fields
    Inline t        -> pure $ Newtype defName params t
  where
    go :: S.SchemaType -> m DelayedType
    go = \case
      S.SParam (S.VarName x) -> pure $ Inline (HRef (VarName x))
      S.SNamed n args -> do
        ref <- HGlobal <$> genName n
        args' <- traverse (fromDelayed <=< go) args
        pure $ Inline $ foldl HApp ref args'
      S.SPrim p -> Inline <$> genBaseType p
      S.SArrow args ret -> do
        let argNames = "Arg" : map (\n -> T.pack $ "Arg" ++ show n) [2 :: Int ..]
        args' <- zipWithM (\name ty -> withSuffix name (fromDelayed =<< go ty)) argNames args
        ret'  <- withSuffix "Ret" (fromDelayed =<< go ret)
        pure $ Inline $ foldr (\x rest -> HArrow x rest) ret' args'

      S.STuple els -> do
        els' <- traverse (fromDelayed <=< go) els
        pure $ Inline $ HTuple els'

      S.SArray el -> do
        a <- go el >>= fromDelayed
        pure $ Inline $ HApp HList a

      S.SRecord fields -> MkRecord <$> traverse genField fields
      S.SVariant ctors -> MkSumType <$> traverse genCtor ctors

    genField :: (MonadGen m) => (S.FieldName, S.FieldOpt, S.SchemaType) -> m (FieldName, HaskellType)
    genField (S.FieldName lbl, opt, sty) = do
      ty' <- withSuffix lbl $ fromDelayed =<< go sty
      pure
        ( FieldName lbl
        , case opt of
            S.RequiredField -> ty'
            S.OptionalField -> HApp HMaybe ty'
        )

    genCtor :: (MonadGen m) => (S.CtorName, Maybe S.SchemaType) -> m (CtorName, Maybe (Either [(FieldName, HaskellType)] HaskellType))
    genCtor (S.CtorName lbl, sty) = do
        let ctorName = CtorName lbl
        delayed' <- traverse go sty
        case delayed' of
          Just (MkRecord fields) ->
            pure (ctorName, Just (Left fields))
          Just other -> do
            ty <- withSuffix lbl $ withSuffix "Payload" $ fromDelayed other
            pure (ctorName, Just (Right ty))
          Nothing ->
            pure (ctorName, Nothing)

genParams :: forall m. (MonadGen m) => [S.VarName] -> m [VarName]
genParams = pure . map (\(S.VarName n) -> VarName n)

genDefinition :: (MonadGen m) => S.SchemaDef -> m HaskellDef
genDefinition (S.SchemaDef name params body) = do
  params' <- genParams params
  name' <- genName name
  forDefinition name' params' $ genDefBody body

collectDefinitions :: (MonadGen m) => [S.SchemaDef] -> m [HaskellDef]
collectDefinitions defs = do
  traverse (genDefinition >=> register) defs
  defs <- gets (reverse . sRegisteredOrd)
  fmap concat . forM defs $ \def ->
    gets (maybeToList . M.lookup def . sRegistered)

----------------------------------------------------------------------
-- Pretty-printing

ppTypeName :: Name -> Doc AnsiStyle
ppTypeName (Name parts) =
  aConstructor $ hcat $ intersperse "'" $ map pretty (reverse parts)

ppCtorName :: CtorName -> Doc AnsiStyle
ppCtorName (CtorName name) = pretty name

ppFieldName :: FieldName -> Doc AnsiStyle
ppFieldName (FieldName name) = pretty name

ppVarName :: VarName -> Doc AnsiStyle
ppVarName (VarName name) = aVariable $ pretty name

ppHaskellType :: HaskellType -> Doc AnsiStyle
ppHaskellType t =
  case spine t of
    (HGlobal n, args) -> app (ppTypeName n) args
    (HRef x, args) -> app (ppVarName x) args
    (HArrow a b, []) -> parens (ppHaskellType a <+> aConstructor ":~>" <+> ppHaskellType b)
    (HTuple els, []) -> tupled (map ppHaskellType els)
    (HUnit, []) -> aConstructor "()"
    (HVoid, []) -> aConstructor "X.Void"
    (HBool, []) -> aConstructor "X.Bool"
    (HInt, []) -> aConstructor "X.Int"
    (HFloat, []) -> aConstructor "X.Double"
    (HString, []) -> aConstructor "X.Text"
    (HMaybe, [a]) -> parens (aConstructor "X.Maybe" <+> ppHaskellType a)
    (HList, [a]) -> brackets (ppHaskellType a)
    (HDict, [a]) -> parens (aConstructor "X.Map" <+> aConstructor "X.Text" <+> ppHaskellType a)
    (HNat, [_]) -> aConstructor "X.Int"
    _ -> error $ "cpretty: invalid spine: " ++ show t
    where
      app :: Doc AnsiStyle -> [HaskellType] -> Doc AnsiStyle
      app f [] = f
      app f as = parens (f <+> hsep (map cpretty as))

ppHaskellDef :: HaskellDef -> Doc AnsiStyle
ppHaskellDef = \case
  Newtype name params body ->
    vsep [ group $ nest 2 $
             aKeyword "newtype" <+>
             hsep (ppTypeName name : map ppVarName params) <+> "=" <+>
             ppTypeName name <> line <>
             lbrace <+> unAnnotate ("get" <> ppTypeName name) <+> "::" <+> group (cpretty body) <+> rbrace
         , indent 2 $ ppDeriving ["X.Eq", "X.Show"]
         ]

  Record name params [] ->
    vsep [ aKeyword "data" <+>
           hsep (ppTypeName name : map ppVarName params) <+> "=" <+>
           ppTypeName name <+> "{}"
         , indent 2 $ ppDeriving ["X.Eq", "X.Show"]
         ]

  Record name params fields ->
    vsep [ aKeyword "data" <+>
           hsep (ppTypeName name : map ppVarName params) <+> "=" <+>
           ppTypeName name
         , indent 2 $ vsep $
             zipWith (<+>) (lbrace : repeat comma) (map (ppField name) fields) ++
             [rbrace <+> ppDeriving ["X.Eq", "X.Show"]]
         ]

  SumType name params ctors ->
    vsep [ aKeyword "data" <+>
           hsep (ppTypeName name : map ppVarName params)
         , indent 2 $ vsep $
             zipWith (<+>) ("=" : repeat "|") (map (ppCtor name) ctors)
         , indent 4 $ ppDeriving ["X.Eq", "X.Show"]
         ]
  where
    ppField _name (fld, ty) =
      ppFieldName fld <+> "::" <+> cpretty ty

    ppCtor name (ctor, payload) =
      let uniquifiedCtor = unAnnotate $ ppTypeName name <> "'" <> ppCtorName ctor
      in case payload of
           Nothing ->
             uniquifiedCtor
           Just (Right ty) ->
             uniquifiedCtor <+> ppHaskellType ty
           Just (Left []) ->
             uniquifiedCtor
           Just (Left fields) ->
             vsep [ uniquifiedCtor
                  , indent 4 $ vsep $
                    zipWith (<+>) (lbrace : repeat comma) (map (ppField name) fields) ++ [rbrace]
                  ]

    ppDeriving classes =
      aKeyword "deriving" <+> parens (hsep $ punctuate comma $ map aConstructor classes)

ppSType :: Doc AnsiStyle -> SType -> Doc AnsiStyle
ppSType namePrefix = group . go
  where
    ctor = (namePrefix <>)
    ppKind = \case
      Star -> ctor "Star"
      Row -> ctor "Row"
      Presence -> ctor "Presence"
      Nat -> ctor "Nat"
      Arr a v b -> group $ parens (fillSep [ctor "Arr", ppKind a, ppVariance v, ppKind b])

    ppVariance = \case
      Covariant -> ctor "Covariant"
      Contravariant -> ctor "Contravariant"
      Invariant -> ctor "Invariant"

    ppBaseType = \case
      TUnit -> ctor "TUnit"
      TVoid -> ctor "TVoid"
      TBool -> ctor "TBool"
      TInt -> ctor "TInt"
      TFloat -> ctor "TFloat"
      TString -> ctor "TString"
      TDict -> ctor "TDict"
      TNat -> ctor "TNat"

    go = \case
      TRef n -> parens (ctor "TRef" <+> pretty n)
      TBase bt -> group $ parens (fillSep [ctor "TBase", ppBaseType bt])
      TArrow a b -> group $ parens (fillSep [ctor "TArrow", go a, go b])
      TRecord r -> group $ parens (fillSep [ctor "TRecord", go r])
      TVariant r -> group $ parens (fillSep [ctor "TVariant", go r])
      TArray a n -> group $ parens (fillSep [ctor "TArray", go a, go n])
      TPair a b -> group $ parens (fillSep [ctor "TPair", go a, go b])
      TPresent -> ctor "TPresent"
      TAbsent -> ctor "TAbsent"
      TExtend (Label lbl) p a r -> group $ parens (fillSep [ctor "TExtend", parens (ctor "mkLabel" <+> pretty (show lbl)), go p, go a, go r])
      TNil -> ctor "TNil"
      TApp f a -> group $ parens (fillSep [ctor "TApp", go f, go a])
      TForall k b -> group $ parens (fillSep [ctor "TForall", ppKind k, go b])
      TExists k b -> group $ parens (fillSep [ctor "TExists", ppKind k, go b])
      TMu b -> group $ parens (fillSep [ctor "TMu", go b])

genHaskell :: [S.SchemaDef] -> Maybe SType -> Either (Doc AnsiStyle) (Doc AnsiStyle)
genHaskell schemaDefs rootTy = do
  defs <- runGen $ collectDefinitions schemaDefs
  -- let modName = T.intercalate "." $ map (\(S.ModuleName n) -> n) (S._modName modul)
  let modName = T.pack "Test"
  pure $ vsep
    [ "{-# LANGUAGE DuplicateRecordFields      #-}"
    , "{-# LANGUAGE GeneralisedNewtypeDeriving #-}"
    , "{-# LANGUAGE TemplateHaskell            #-}"
    , "{-# LANGUAGE TypeOperators              #-}"
    , mempty
    , "{-# OPTIONS_GHC -fno-warn-unused-imports   #-}"
    , "{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}"
    , mempty
    , "----------------------------------------------------------------------"
    , "--                  Do not modify. Generated code"
    , "----------------------------------------------------------------------"
    , mempty
    , aKeyword "module" <+> aConstructor (pretty modName) <+> aKeyword "where"
    , mempty
    , ppImport "Prelude" "X"
    , ppImport "Data.Void" "X"
    , ppImport "Data.Map" "X"
    , ppImport "Data.Text" "X"
    , ppImport "STL.Runtime" "R"
    , ppImportUnqualified "STL.Runtime" ["(:~>)"]
    , mempty
    , vsep $ punctuate line $ map cpretty defs
    , mempty
    , case rootTy of
        Nothing -> mempty
        Just ty -> ppTypeOfInstance (Name ["API"]) ty
    , mempty
    , vsep $ map ppDeriveJSON defs
    ]
  where
    ppImport :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
    ppImport mod asMod =
      aKeyword "import qualified" <+> aConstructor mod <+>
      aKeyword "as" <+> aConstructor asMod

    ppImportUnqualified :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
    ppImportUnqualified mod what =
      aKeyword "import" <+> aConstructor mod <+> parens (hsep $ punctuate comma what)

    ppTypeOfInstance :: Name -> SType -> Doc AnsiStyle
    ppTypeOfInstance name ty = vsep
      [ aKeyword "instance" <+> aConstructor "R.TypeOf" <+> ppTypeName name <+> aKeyword "where"
      , indent 2 $ "typeOf _" <+> "="
      , indent 4 $ ppSType "R." ty
      ]

    ppDeriveJSON :: HaskellDef -> Doc AnsiStyle
    ppDeriveJSON def =
      "R.deriveSerialisation" <+>
         isSumType def <+>
         dquotes (ppTypeName (hdefName def)) <+>
         squote <> squote <> ppTypeName (hdefName def)

    isSumType = \case
      SumType{} -> "X.True"
      _         -> "X.False"
