{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module STL.Syntax.Types where

import Data.Functor.Foldable (Fix(..))
import Data.Text
import STL.Syntax.Position

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Show, Eq, Ord)

newtype Var = Var Text
  deriving (Show, Eq, Ord)

newtype GlobalName = GlobalName Text
  deriving (Show, Eq, Ord)

newtype ModuleName = ModuleName Text
  deriving (Show, Eq, Ord)

newtype Label = Label Text
  deriving (Show, Eq, Ord)

data Kind
  = Star
  | Nat
  | Row
  | Arr Kind Variance Kind
  deriving (Show, Eq, Ord)

data BaseType
  = TUnit       -- Star
  | TVoid       -- Star
  | TBool       -- Star
  | TInt        -- Star
  | TFloat      -- Star
  | TString     -- Star
  | TDict       -- Star -> Star
  | TNat        -- Nat -> Star
  deriving (Show, Eq, Ord)

data Binding v = Binding
  { _bndPos  :: Position
  , _bndVar  :: Var
  , _bdnKind :: Maybe Kind
  , _bdnVariance :: v
  } deriving (Show, Eq, Ord)

data TypeF e
  = T         { _typePos :: Position, _baseType :: BaseType }
  | TRef      { _typePos :: Position, _refName :: Var }
  | TGlobal   { _typePos :: Position, _globalQualifiers :: Maybe ModuleName, _globalName :: GlobalName }
  | TForall   { _typePos :: Position, _forallBindings :: [Binding ()], _forallBody :: e }
  | TExists   { _typePos :: Position, _existsBindings :: [Binding ()], _existsBody :: e }
  | TArrow    { _typePos :: Position, _arrA :: e, _arrB :: e, _arrRest :: [e] }
  | TApp      { _typePos :: Position, _appF :: e, _appA :: e, _appRest :: [e] }

  | TRecord   { _typePos :: Position, _recMixin :: e, _recFunctor :: Maybe e }
  | TVariant  { _typePos :: Position, _vntMixin :: e, _vntFunctor :: Maybe e }

  | TMixin    { _typePos :: Position, _mixinFunctor :: Maybe (Binding ()), _mixinRow :: Row e }
  | TUnion    { _typePos :: Position, _unionA :: e, _unionB :: e, _unionRest :: [e] }

  | TTuple    { _typePos :: Position, _tupleH :: e, _tupleRest :: [e] }
  | TArray    { _typePos :: Position, _arrayElem :: e, _arraySize :: e }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Type = Fix TypeF

typePos :: Type -> Position
typePos (Fix t) = _typePos t

data Presence
  = PPresent  { _presencePos :: Position }
  | PVariable { _presencePos :: Position }
  deriving (Show, Eq, Ord)

data Row t
  = RExtend   { _rowPos :: Position, _extLabel :: Label, _extPresence :: Presence, _extType :: t, _extCont :: Row t }
  | RNil      { _rowPos :: Position }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance {-# OVERLAPPING #-} Show (Fix TypeF) where
  showsPrec n (Fix t) = showsPrec n t

data MutualClause = MutualClause
  { _mutPos  :: Position
  , _mutName :: GlobalName
  , _mutBody :: Type
  } deriving (Show)

data Statement
  = Typedef   { _stmtPos :: Position, _defnName :: GlobalName, _defnParams :: [Binding Variance], _defnBody :: Type }
  | Mutualdef { _stmtPos :: Position, _mutParams :: [Binding Variance], _mutClauses :: [MutualClause] }
  | Normalise { _stmtPos :: Position, _normaliseBody :: Type }
  | Subsume   { _stmpPos :: Position, _subType :: Type, _superType :: Type }
  deriving (Show)

data Import = Import
  { _importPos    :: Position
  , _importName   :: [ModuleName]
  , _importRename :: Maybe [ModuleName]
  } deriving (Show)

data Module = Module
  { _modName       :: [ModuleName]
  , _modImport     :: [Import]
  , _modStatements :: [Statement]
  , _modReturnType :: Maybe Type
  } deriving (Show)
