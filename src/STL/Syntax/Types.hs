{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module STL.Syntax.Types where

import Data.Functor.Foldable (Fix(..))
import Data.Text
import STL.Syntax.Position

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
  | Presence
  | Arr Kind Kind
  deriving (Show, Eq, Ord)

data BaseType
  = TUnit       -- Star
  | TVoid       -- Star
  | TInteger    -- Star
  | TDouble     -- Star
  | TString     -- Star
  | TList       -- Star -> Star
  | TDictionary -- Star -> Star
  | TArray      -- Star -> Nat -> Star
  | TNatural    -- Nat -> Star
  | TPresent    -- Precense
  | TAbsent     -- Presence
  deriving (Show, Eq, Ord)

data Binding = Binding
  { _bndPos  :: Position
  , _bndVar  :: Var
  , _bdnKind :: Maybe Kind
  } deriving (Show, Eq, Ord)

data TypeF e
  = T         { _typePos :: Position, _baseType :: BaseType }
  | TRef      { _typePos :: Position, _refName :: Var }
  | TGlobal   { _typePos :: Position, _globalQualifiers :: Maybe ModuleName, _globalName :: GlobalName }
  | TForall   { _typePos :: Position, _forallBindings :: [Binding], _forallBody :: e }
  | TArrow    { _typePos :: Position, _arrA :: e, _arrB :: e, _arrRest :: [e] }
  | TApp      { _typePos :: Position, _appF :: e, _appA :: e, _appRest :: [e] }
  | TRecord   { _typePos :: Position, _recordRow :: Row e  }
  | TVariant  { _typePos :: Position, _variantRow :: Row e }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Type = Fix TypeF

getPosition :: Type -> Position
getPosition (Fix t) = _typePos t

data Row t
  = RExtend   { _rowPos :: Position, _extLabel :: Label, _extPresence :: t, _extType :: t, _extCont :: Row t }
  | RNil      { _rowPos :: Position }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance {-# OVERLAPPING #-} Show (Fix TypeF) where
  showsPrec n (Fix t) = showsPrec n t

data Statement
  = Define    { _stmtPos :: Position, _defnName :: GlobalName, _defnParams :: [Binding] , _defnBody :: Type }
  | Normalise { _stmtPos :: Position, _normaliseBody :: Type }
  | Subsume   { _stmpPos :: Position, _subType :: Type, _superType :: Type }
  deriving (Show)

data Import = Import
  { _importPos    :: Position
  , _importName   :: ModuleName
  , _importArgs   :: [Type]
  , _importRename :: ModuleName
  } deriving (Show)

data Module = Module
  { _modName       :: ModuleName
  , _modParams     :: [Binding]
  , _modImport     :: [Import]
  , _modStatements :: [Statement]
  , _modReturnType :: Type
  } deriving (Show)
