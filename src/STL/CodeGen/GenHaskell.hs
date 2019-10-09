{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.CodeGen.GenHaskell (genHaskell) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Foldable (cata)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)

import STL.Pretty
import qualified STL.Syntax as S
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
    , hdefCases :: [(CtorName, Maybe HaskellType)]
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
  , sNameMapping   :: Map S.GlobalName Name
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

genName :: (MonadGen m) => S.GlobalName -> m Name
genName global@(S.GlobalName name) = do
  mapping <- gets sNameMapping
  case M.lookup global mapping of
    Just known -> pure known
    Nothing    -> do
      new <- refreshName $ Name [name]
      modify (\s -> s { sNameMapping = M.insert global new (sNameMapping s) })
      pure new

genBaseType :: MonadGen m => S.BaseType -> m HaskellType
genBaseType = \case
  S.TUnit   -> pure HUnit
  S.TVoid   -> pure HVoid
  S.TBool   -> pure HBool
  S.TInt    -> pure HInt
  S.TFloat  -> pure HFloat
  S.TString -> pure HString
  S.TList   -> pure HList
  S.TDict   -> pure HDict
  S.TNat    -> pure HNat

data DelayedType
  = ViaDef (Name -> [VarName] -> HaskellDef)
  | Inline HaskellType

applyParams :: HaskellType -> [VarName] -> HaskellType
applyParams = foldl (\a v -> HApp a (HRef v))

fromDelayed :: MonadGen m => DelayedType -> m HaskellType
fromDelayed = \case
  ViaDef f -> do
    name <- refreshName =<< asks eDerivedName
    params <- asks eParameters
    register $ f name params
    pure $ applyParams (HGlobal name) params
  Inline t -> pure t

genDefBody :: forall m. MonadGen m => [Name] -> S.Type -> m HaskellDef
genDefBody recursionClauses ty = do
  delayed <- cata alg ty
  defName <- asks eDerivedName
  params  <- asks eParameters
  case delayed of
    ViaDef f -> pure $ f defName params
    Inline t -> pure $ Newtype defName params t
  where
    alg :: S.TypeF (m DelayedType) -> m DelayedType
    alg = \case
      S.T _ bt ->
        Inline <$> genBaseType bt

      S.TRef _ (S.Var x) ->
        pure $ Inline $ HRef (VarName x)

      S.TGlobal _ Nothing name -> do
        name' <- genName name
        if name' `elem` recursionClauses
          then do
            params  <- asks eParameters
            pure $ Inline $ applyParams (HGlobal name') params
          else pure $ Inline $ HGlobal name'

      S.TGlobal pos (Just _mod) _name ->
        throwError $ pretty pos <> ": fully qualified names not supported"

      S.TForall pos _ _ ->
        throwError $ pretty pos <> ": universal quantificaiton not supported"

      S.TExists pos _ _ ->
        throwError $ pretty pos <> ": existential quantification not supported"

      S.TArrow _ a b cs -> do
        a' <- withSuffix "Argument" $ fromDelayed =<< a
        b' <- withSuffix "Result" $ fromDelayed =<< b
        cs' <- mapM fromDelayed =<< sequence cs
        pure $ Inline $ foldr (\x rest -> HArrow x rest) (last (a' : b' : cs')) (init (a' : b' : cs'))

      S.TApp _ f a as -> do
        f' <- fromDelayed =<< f
        a' <- fromDelayed =<< a
        as' <- mapM fromDelayed =<< sequence as
        pure $ Inline $ foldl HApp f' (a' : as')

      S.TRecord _ row -> do
        fields <- genFields row
        pure $ ViaDef $ \name params -> Record name params fields

      S.TVariant _ row -> do
        ctors <- genCtors row
        pure $ ViaDef $ \name params -> SumType name params ctors

      S.TArray pos _ _ ->
        throwError $ pretty pos <> ": Nat-indexed arrays not supported"

genFields :: (MonadGen m) => S.Row (m DelayedType) -> m [(FieldName, HaskellType)]
genFields = \case
  S.RNil _ -> pure []
  S.RExplicit pos _ -> throwError $ pretty pos <> ": explicit record tail not supported"
  S.RExtend _ (S.Label lbl) prs ty rest -> do
    ty' <- withSuffix lbl $ fromDelayed =<< ty
    let fname = FieldName lbl
    let isOptional = case prs of
          S.PPresent{} -> False
          S.PVariable{} -> True
    rest' <- genFields rest
    pure $ (fname, if isOptional then HApp HMaybe ty' else ty') : rest'

genCtors :: (MonadGen m) => S.Row (m DelayedType) -> m [(CtorName, Maybe HaskellType)]
genCtors = \case
  S.RNil _ -> pure []
  S.RExplicit pos _ -> throwError $ pretty pos <> ": explicit variant tail not supported"
  S.RExtend _ (S.Label lbl) _prs ty rest -> do
    ty' <- withSuffix "Payload" $ withSuffix lbl $ fromDelayed =<< ty
    let fname = CtorName lbl
    let cty = case ty' of
          HUnit -> Nothing
          other -> Just other
    rest' <- genCtors rest
    pure $ (fname, cty) : rest'

genParams :: forall m. (MonadGen m) => [S.Binding S.Variance] -> m [VarName]
genParams = mapM genParam
  where
    isFancy :: S.Kind -> Bool
    isFancy = \case
      S.Star -> False
      _other -> True

    genParam :: S.Binding S.Variance -> m VarName
    genParam (S.Binding pos (S.Var x) k _) =
      case k of
        Just k' | isFancy k' -> throwError $ pretty pos <> ": only Type kind currently supported"
        _ -> pure (VarName x)

genDefinition :: (MonadGen m) => S.GlobalName -> [S.Binding S.Variance] -> [S.GlobalName] -> S.Type -> m HaskellDef
genDefinition name params recursionClauses body = do
  params' <- genParams params
  name' <- genName name
  recursionClauses' <- mapM genName recursionClauses
  forDefinition name' params' $ genDefBody recursionClauses' body

collectDefinitions :: (MonadGen m) => S.Module -> m [HaskellDef]
collectDefinitions modul = do
  forM_ (S._modStatements modul) $ \case
    S.Typedef _ name params body -> do
      def <- genDefinition name params [name] body
      register def
    S.Mutualdef _ params clauses -> do
      let names = map (\(S.MutualClause _ name _) -> name) clauses
      defs <- forM clauses $ \(S.MutualClause _ name body) ->
        genDefinition name params names body
      mapM_ register defs
    S.Normalise{} -> pure ()
    S.Subsume{} -> pure ()

  case S._modReturnType modul of
    Nothing -> pure ()
    Just api -> forDefinition (Name ["API"]) [] $ genDefBody [] api >>= register

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
    vsep [ aKeyword "newtype" <+>
           ppTypeName name <+>
           ppParams params <+> "=" <+>
           ppTypeName name <+> cpretty body
         , indent 2 $ ppDeriving ["X.Eq", "X.Show"]
         ]

  Record name params fields ->
    vsep [ aKeyword "data" <+>
           ppTypeName name <+>
           ppParams params <+> "=" <+> ppTypeName name
         , indent 2 $ vsep $
             zipWith (<+>) (lbrace : repeat comma) (map (ppField name) fields) ++
             [rbrace <+> ppDeriving ["X.Eq", "X.Show"]]
         ]

  SumType name params ctors ->
    vsep [ aKeyword "data" <+>
           ppTypeName name <+>
           ppParams params
         , indent 2 $ vsep $
             zipWith (<+>) ("=" : repeat "|") (map (ppCtor name) ctors)
         , indent 4 $ ppDeriving ["X.Eq", "X.Show"]
         ]
  where
    ppParams params =
      hsep (map ppVarName params)

    ppField name (fld, ty) =
      let uniquifiedField = unAnnotate $ "_" <> ppTypeName name <> "_" <> ppFieldName fld
      in uniquifiedField <+> "::" <+> cpretty ty

    ppCtor name (ctor, payload) =
      let uniquifiedCtor = unAnnotate $ ppTypeName name <> "'" <> ppCtorName ctor
      in case payload of
           Nothing -> uniquifiedCtor
           Just ty -> uniquifiedCtor <+> ppHaskellType ty

    ppDeriving classes =
      aKeyword "deriving" <+> parens (hsep $ punctuate comma $ map aConstructor classes)

genHaskell :: S.Module -> Maybe SType -> Either (Doc AnsiStyle) (Doc AnsiStyle)
genHaskell modul _rootTy = do
  defs <- runGen $ collectDefinitions modul
  let modName = T.intercalate "." $ map (\(S.ModuleName n) -> n) (S._modName modul)
  pure $ vsep
    [ "{-# LANGUAGE GeneralisedNewtypeDeriving #-}"
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
    , ppImport "Runtime" "R"
    , ppImportUnqualified "Runtime" ["(:~>)"]
    , mempty
    , vsep $ punctuate line $ map cpretty defs
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

    ppDeriveJSON :: HaskellDef -> Doc AnsiStyle
    ppDeriveJSON def =
      "R.deriveSerialisation" <+>
         isSumType def <+>
         dquotes (ppTypeName (hdefName def)) <+>
         squote <> squote <> ppTypeName (hdefName def)

    isSumType = \case
      SumType{} -> "X.True"
      _         -> "X.False"
