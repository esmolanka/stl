{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Data.StructuralType where

import Control.Arrow

import Data.Coerce
import Data.Functor.Foldable
import qualified Data.Text as T
import Data.Text (Text)

import GHC.Generics
import qualified STL.Core.Eval as Core
import qualified STL.Core.Types as Core

newtype Label = Label Text
  deriving (Show, Eq, Ord)

data BaseType
  = TUnit
  | TVoid
  | TBool
  | TInt
  | TFloat
  | TString
  | TList
  | TDict
  | TNat
  | TPair
  deriving (Show, Eq, Ord, Generic)

data Kind
  = Star
  | Row
  | Presence
  | Nat
  | Arr Kind Variance Kind
  deriving (Show, Eq, Ord, Generic)

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Show, Eq, Ord, Generic)

data SType
  = TRef Int
  | TBase BaseType
  | TArrow SType SType
  | TRecord SType
  | TVariant SType
  | TArray SType SType
  | TPresent
  | TAbsent
  | TExtend Label SType SType SType
  | TNil
  | TApp SType SType
  | TForall Kind SType
  | TExists Kind SType
  | TMu SType
  deriving (Show, Eq, Ord, Generic)

extract :: Core.Type -> SType
extract = mkType
  where
    mkType :: Core.Type -> SType
    mkType ty =
      case first (coerce :: Core.Type -> Core.TypeF Core.Type) (Core.spine ty) of
        (Core.TRef _ x n, args)
          | x == dummyVar -> mkApps (TRef n) args
        (Core.TBase _ bt, args) ->
          mkApps (TBase (mkBaseType bt)) args
        (Core.TArrow{}, [a, b]) ->
          TArrow (extract a) (extract b)
        (Core.TRecord{}, [r]) ->
          TRecord (extract r)
        (Core.TVariant{}, [r]) ->
          TVariant (extract r)
        (Core.TArray{}, [a, n]) ->
          TArray (extract a) (extract n)
        (Core.TPresent{}, []) ->
          TPresent
        (Core.TAbsent{}, []) ->
          TAbsent
        (Core.TExtend _ lbl, [p,a,r]) ->
          TExtend (mkLabel lbl) (extract p) (extract a) (extract r)
        (Core.TNil{}, []) ->
          TNil
        (Core.TForall _ x k b, []) ->
          TForall (mkKind k) (extract (renameVar x b))
        (Core.TExists _ x k b, []) ->
          TExists (mkKind k) (extract (renameVar x b))
        (Core.TMu _ x b, []) ->
          TMu (extract (renameVar x b))
        _other ->
          error "Invalid constructor"

    dummyVar :: Core.Var
    dummyVar = Core.Var (T.pack "~")

    renameVar :: Core.Var -> Core.Type -> Core.Type
    renameVar x =
      Core.subst x 0 (Fix $ Core.TRef Core.dummyPos dummyVar 0) .
      Core.shift 1 dummyVar

    mkApps :: SType -> [Core.Type] -> SType
    mkApps = foldl (\a t -> TApp a (extract t))

    mkBaseType :: Core.BaseType -> BaseType
    mkBaseType = \case
      Core.TUnit   -> TUnit
      Core.TVoid   -> TVoid
      Core.TBool   -> TBool
      Core.TInt    -> TInt
      Core.TFloat  -> TFloat
      Core.TString -> TString
      Core.TList   -> TList
      Core.TDict   -> TDict
      Core.TNat    -> TNat
      Core.TPair   -> TPair

    mkKind :: Core.Kind -> Kind
    mkKind = \case
      Core.Star -> Star
      Core.Row -> Row
      Core.Presence -> Presence
      Core.Nat -> Nat
      Core.Arr f v a -> Arr (mkKind f) (mkVariance v) (mkKind a)

    mkVariance :: Core.Variance -> Variance
    mkVariance = \case
      Core.Covariant -> Covariant
      Core.Contravariant -> Contravariant
      Core.Invariant -> Invariant

    mkLabel :: Core.Label -> Label
    mkLabel (Core.Label lbl) = Label lbl

inject :: SType -> Core.Type
inject = mkType
  where
    mkType :: SType -> Core.Type
    mkType = Fix . \case
      TRef n -> Core.TRef pos dummyVar n
      TBase bt -> Core.TBase pos (mkBaseType bt)
      TArrow a b -> Core.TArrow pos $$ a $$ b
      TRecord r -> Core.TRecord pos $$ r
      TVariant r -> Core.TVariant pos $$ r
      TArray a n -> Core.TArray pos $$ a $$ n
      TPresent -> Core.TPresent pos
      TAbsent -> Core.TAbsent pos
      TExtend lbl p a r -> Core.TExtend pos (mkLabel lbl) $$ p $$ a $$ r
      TNil -> Core.TNil pos
      TApp f a -> Core.TApp pos (mkType f) (mkType a)
      TForall k b -> Core.TForall pos dummyVar (mkKind k) (mkType b)
      TExists k b -> Core.TExists pos dummyVar (mkKind k) (mkType b)
      TMu b -> Core.TMu pos dummyVar (mkType b)

    pos :: Core.Position
    pos = Core.dummyPos

    dummyVar :: Core.Var
    dummyVar = Core.Var (T.pack "α")

    ($$) :: Core.TypeF Core.Type -> SType -> Core.TypeF Core.Type
    ($$) f a = Core.TApp pos (Fix f) (mkType a)
    infixl 3 $$

    mkBaseType :: BaseType -> Core.BaseType
    mkBaseType = \case
      TUnit   -> Core.TUnit
      TVoid   -> Core.TVoid
      TBool   -> Core.TBool
      TInt    -> Core.TInt
      TFloat  -> Core.TFloat
      TString -> Core.TString
      TList   -> Core.TList
      TDict   -> Core.TDict
      TNat    -> Core.TNat
      TPair   -> Core.TPair

    mkKind :: Kind -> Core.Kind
    mkKind = \case
      Star -> Core.Star
      Row -> Core.Row
      Presence -> Core.Presence
      Nat -> Core.Nat
      Arr f v a -> Core.Arr (mkKind f) (mkVariance v) (mkKind a)

    mkVariance :: Variance -> Core.Variance
    mkVariance = \case
      Covariant -> Core.Covariant
      Contravariant -> Core.Contravariant
      Invariant -> Core.Invariant

    mkLabel :: Label -> Core.Label
    mkLabel (Label lbl) = Core.Label lbl
