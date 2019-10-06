{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module GenHaskell (genHaskell, process) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import STL.Pretty hiding (list)
import qualified STL.Syntax as S

----------------------------------------------------------------------
-- Target types

newtype Name = Name Text deriving (Show, Eq, Ord, Pretty)
newtype FieldName = FieldName Text deriving (Show, Eq, Ord, Pretty)
newtype CtorName = CtorName Text deriving (Show, Eq, Ord, Pretty)
newtype VarName  = VarName Text deriving (Show, Eq, Ord, Pretty)

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
  | ADT
    { hdefName :: Name
    , hdefParams :: [VarName]
    , hdefCases :: [(CtorName, HaskellType)]
    }
  deriving (Show, Eq, Ord)

instance CPretty HaskellDef where
  cpretty = ppHaskellDef

----------------------------------------------------------------------
-- Generation monad

type MonadGen m =
  ( MonadState GenState m
  , MonadError (Doc AnsiStyle) m
  )

data GenState = GenState
  { sRegistered    :: Map Name HaskellDef
  , sRegisteredOrd :: [Name]
  , sNameMapping   :: Map S.GlobalName Name
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
  pure $ head $ filter (not . flip S.member used) (map Name $ iterate (\m -> m <> "'") n)

runGen :: ExceptT (Doc AnsiStyle) (State GenState) a -> Either (Doc AnsiStyle) a
runGen a =
  evalState
    (runExceptT a)
    (GenState M.empty [] M.empty)

----------------------------------------------------------------------

genName :: (MonadGen m) => S.GlobalName -> m Name
genName global@(S.GlobalName name) = do
  mapping <- gets sNameMapping
  case M.lookup global mapping of
    Just known -> pure known
    Nothing    -> do
      new <- refreshName $ Name name
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

genDefBody :: forall m. MonadGen m => Name -> [VarName] -> S.Type -> m HaskellDef
genDefBody defName params ty = do
  delayed <- cata alg ty
  case delayed of
    ViaDef f -> pure $ f defName params
    Inline t -> pure $ Newtype defName params t
  where
    applyParams :: HaskellType -> [VarName] -> HaskellType
    applyParams = foldl (\a v -> HApp a (HRef v))

    fromDelayed :: DelayedType -> m HaskellType
    fromDelayed = \case
      ViaDef f -> do
        name <- refreshName defName
        register $ f name params
        pure $ applyParams (HGlobal name) params
      Inline t -> pure t

    alg :: S.TypeF (m DelayedType) -> m DelayedType
    alg = \case
      S.T _ bt ->
        Inline <$> genBaseType bt

      S.TRef _ (S.Var x) ->
        pure $ Inline $ HRef (VarName x)

      S.TGlobal _ Nothing name ->
        Inline . HGlobal <$> genName name

      S.TGlobal pos (Just _mod) _name ->
        throwError $ pretty pos <> ": fully qualified names are not supported"

      S.TForall pos _ _ ->
        throwError $ pretty pos <> ": universal quantificaiton not supported"

      S.TExists pos _ _ ->
        throwError $ pretty pos <> ": existential quantification not supported"

      S.TArrow _ a b cs -> do
        a' <- fromDelayed =<< a
        b' <- fromDelayed =<< b
        cs' <- mapM fromDelayed =<< sequence cs
        pure $ Inline $ foldr (\x rest -> HArrow x rest) (last (a' : b' : cs')) (init (a' : b' : cs'))

      S.TApp _ f a as -> do
        f' <- fromDelayed =<< f
        a' <- fromDelayed =<< a
        as' <- mapM fromDelayed =<< sequence as
        pure $ Inline $ foldl HApp f' (a' : as')

      S.TRecord _ row -> do
        row' <- mapM fromDelayed =<< sequence row
        fields <- genFields row'
        pure $ ViaDef $ \name params -> Record name params fields

      S.TVariant _ row -> do
        row' <- mapM fromDelayed =<< sequence row
        ctors <- genCtors row'
        pure $ ViaDef $ \name params -> ADT name params ctors

      S.TArray pos _ _ ->
        throwError $ pretty pos <> ": nat-indexed arrays not supported"

genFields :: (MonadGen m) => S.Row HaskellType -> m [(FieldName, HaskellType)]
genFields = \case
  S.RNil _ -> pure []
  S.RExplicit pos _ -> throwError $ pretty pos <> ": explicit record tail not supported"
  S.RExtend _ (S.Label lbl) prs ty rest -> do
    let isOptional = case prs of
          S.PPresent{} -> False
          S.PVariable{} -> True
    let fname = FieldName lbl
    rest' <- genFields rest
    pure $ (fname, if isOptional then HApp HMaybe ty else ty) : rest'

genCtors :: (MonadGen m) => S.Row HaskellType -> m [(CtorName, HaskellType)]
genCtors = \case
  S.RNil _ -> pure []
  S.RExplicit pos _ -> throwError $ pretty pos <> ": explicit variant tail not supported"
  S.RExtend _ (S.Label lbl) _prs ty rest -> do
    let fname = CtorName lbl
    rest' <- genCtors rest
    pure $ (fname, ty) : rest'

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
        Just k' | isFancy k' -> throwError $ pretty pos <> ": non-Star kinds unsupported"
        _ -> pure (VarName x)

genDefinition :: (MonadGen m) => S.GlobalName -> [S.Binding S.Variance] -> S.Type -> m HaskellDef
genDefinition name params body = do
  params' <- genParams params
  name' <- genName name
  genDefBody name' params' body

collectDefinitions :: (MonadGen m) => S.Module -> m [HaskellDef]
collectDefinitions modul = do
  forM_ (S._modStatements modul) $ \case
    S.Typedef _ name params body -> do
      def <- genDefinition name params body
      register def
    S.Mutualdef _ params clauses -> do
      defs <- forM clauses $ \(S.MutualClause _ name body) ->
        genDefinition name params body
      mapM_ register defs
    S.Normalise{} -> pure ()
    S.Subsume{} -> pure ()

  case S._modReturnType modul of
    Nothing -> pure ()
    Just api -> genDefBody (Name "API") [] api >>= register

  defs <- gets (reverse . sRegisteredOrd)
  fmap concat . forM defs $ \def ->
    gets (maybeToList . M.lookup def . sRegistered)

----------------------------------------------------------------------
-- Pretty-printing

ppHaskellType :: HaskellType -> Doc AnsiStyle
ppHaskellType t =
  case spine t of
    (HGlobal n, args) -> app (aConstructor (pretty n)) args
    (HRef x, args) -> app (aVariable (pretty x)) args
    (HArrow a b, []) -> parens (cpretty a <+> ":~>" <+> cpretty b)
    (HUnit, []) -> "()"
    (HVoid, []) -> "X.Void"
    (HBool, []) -> "X.Bool"
    (HInt, []) -> "X.Int"
    (HFloat, []) -> "X.Double"
    (HString, []) -> "X.Text"
    (HMaybe, []) -> "X.Maybe"
    (HList, [a]) -> brackets (cpretty a)
    (HDict, [a]) -> parens ("X.Map" <+> "X.Text" <+> cpretty a)
    (HNat, [_]) -> "Int"
    _ -> error $ "cpretty: invalid spine: " ++ show t
    where
      app :: Doc AnsiStyle -> [HaskellType] -> Doc AnsiStyle
      app f [] = f
      app f as = parens (f <+> hsep (map cpretty as))

ppTypeName :: Name -> Doc AnsiStyle
ppTypeName name = aConstructor (pretty name)

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

  ADT name params ctors ->
    vsep [ aKeyword "data" <+>
           ppTypeName name <+>
           ppParams params
         , indent 2 $ vsep $
             zipWith (<+>) ("=" : repeat "|") (map (ppCtor name) ctors)
         , indent 4 $ ppDeriving ["X.Eq", "X.Show"]
         ]
  where
    ppParams params = hsep (map (aVariable . pretty) params)
    ppField name (fld, ty) = "_" <> pretty name <> "_" <> pretty fld <+> "::" <+> cpretty ty
    ppCtor name (ctor, payload) = pretty name <> "'" <> pretty ctor <+> cpretty payload
    ppDeriving classes = aKeyword "deriving" <+> parens (hsep $ punctuate comma $ map aConstructor classes)

genHaskell :: S.Module -> Either (Doc AnsiStyle) (Doc AnsiStyle)
genHaskell modul = do
  defs <- runGen $ collectDefinitions modul
  let modName = T.intercalate "." $ map (\(S.ModuleName n) -> n) (S._modName modul)
  pure $ vsep
    [ vsep $ map ppLanguagePragma
        [ "TemplateHaskell"
        , "TypeOperators"
        , "GeneralisedNewtypeDeriving"
        ]
    , mempty
    , aKeyword "module" <+> aConstructor (pretty modName) <+> aKeyword "where"
    , mempty
    , ppImport "Prelude" "X"
    , ppImport "Data.Void" "X"
    , ppImport "Data.Map" "X"
    , ppImport "Data.Aeson" "Aeson"
    , ppImport "Data.Aeson.TH" "Aeson"
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

    ppLanguagePragma :: Doc AnsiStyle -> Doc AnsiStyle
    ppLanguagePragma p =
      "{-# LANGUAGE" <+> p <+> "#-}"

    ppDeriveJSON :: HaskellDef -> Doc AnsiStyle
    ppDeriveJSON def = "Aeson.deriveJSON" <+> parens ("R.jsonOptions" <+> isADT def <+> pretty (let (Name n) = hdefName def in T.length n)) <+> squote <> squote <> ppTypeName (hdefName def)

    isADT = \case
      ADT{} -> "X.True"
      _ -> "X.False"

getModule :: FilePath -> IO (Either (Doc AnsiStyle) S.Module)
getModule fn = do
  str <- liftIO $ BL.readFile fn
  case S.parseModule fn str of
    Left err ->
      pure (Left (pretty err))
    Right modul ->
      pure (Right modul)

process :: FilePath -> IO ()
process fn = getModule fn >>= \x ->
  either (error . show) putDocLn (x >>= genHaskell)

-- >>> runGen $ refreshName (Name (T.pack "foo")) >> refreshName (Name (T.pack "foo"))
-- Right (Name "foo")

-- >>> process "/Users/eugene/Projects/stl/examples/ex1.stl"
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- module Foo where
-- import qualified Prelude as X
-- import qualified Data.Void as X
-- import qualified Data.Map as X
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.TH as Aeson
-- import qualified Runtime as R
-- import Runtime ((:~>))
-- data Maybe a
--   = Maybe'Nothing ()
--   | Maybe'Just a
--     deriving (X.Eq, X.Show)
-- data Pair a b = Pair
--   { _Pair_fst :: a
--   , _Pair_snd :: b
--   } deriving (X.Eq, X.Show)
-- data Foo'  = Foo'
--   { _Foo'_foo :: (X.Int :~> (X.Int :~> X.Int))
--   , _Foo'_bar :: X.Double
--   } deriving (X.Eq, X.Show)
-- data Foo
--   = Foo'Nothing ()
--   | Foo'Something Foo'
--     deriving (X.Eq, X.Show)
-- data Odd a
--   = Odd'OZero a
--   | Odd'OSucc Even
--     deriving (X.Eq, X.Show)
-- data Even a
--   = Even'EZero a
--   | Even'ESucc Odd
--     deriving (X.Eq, X.Show)
-- newtype API  = API (Maybe [(Pair () X.Void)])
--   deriving (X.Eq, X.Show)
-- Aeson.deriveJSON (R.jsonOptions X.True 5) ''Maybe
-- Aeson.deriveJSON (R.jsonOptions X.False 4) ''Pair
-- Aeson.deriveJSON (R.jsonOptions X.False 4) ''Foo'
-- Aeson.deriveJSON (R.jsonOptions X.True 3) ''Foo
-- Aeson.deriveJSON (R.jsonOptions X.True 3) ''Odd
-- Aeson.deriveJSON (R.jsonOptions X.True 4) ''Even
-- Aeson.deriveJSON (R.jsonOptions X.False 3) ''API
