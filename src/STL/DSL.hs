{-# LANGUAGE PatternSynonyms #-}

module STL.DSL where

import Data.Functor.Compose
import Data.Functor.Foldable (Fix(..))
import Data.Text (Text)
import STL.Types

pattern Ref :: Var -> Type
pattern Ref x <- Fix (TRef _ x 0)
  where Ref x = Fix (TRef dummyPos x 0)

pattern Refn :: Var -> Int -> Type
pattern Refn x n <- Fix (TRef _ x n)
  where Refn x n = Fix (TRef dummyPos x n)

pattern Global :: Text -> Type
pattern Global x <- Fix (TGlobal _ (GlobalName x))
  where Global x = Fix (TGlobal dummyPos (GlobalName x))

pattern Unit :: Type
pattern Unit <- Fix (TUnit _)
  where Unit = Fix (TUnit dummyPos)

pattern Void :: Type
pattern Void <- Fix (TVoid _)
  where Void = Fix (TVoid dummyPos)

pattern (:~>) :: Type -> Type -> Type
pattern (:~>) a b <- Fix (TArrow _) :$ a :$ b
  where (:~>) a b = Fix (TArrow dummyPos) :$ a :$ b
infixr 3 :~>

pattern Record :: Type -> Type
pattern Record r <- Fix (TRecord _) :$ r
  where Record r = Fix (TRecord dummyPos) :$ r

pattern Variant :: Type -> Type
pattern Variant r <- Fix (TVariant _) :$ r
  where Variant r = Fix (TVariant dummyPos) :$ r

pattern Present :: Type
pattern Present <- Fix (TPresent _)
  where Present = Fix (TPresent dummyPos)

pattern Absent :: Type
pattern Absent <- Fix (TAbsent _)
  where Absent = Fix (TAbsent dummyPos)

pattern Extend :: Label -> Type -> Type -> Type -> Type
pattern Extend lbl p s r <- Fix (TExtend _ lbl) :$ p :$ s :$ r
  where Extend lbl p s r = Fix (TExtend dummyPos lbl) :$ p :$ s :$ r

pattern Nil :: Type
pattern Nil <- Fix (TNil _)
  where Nil = Fix (TNil dummyPos)

pattern (:$) :: Type -> Type -> Type
pattern (:$) f a <- Fix (TApp _ f a)
  where (:$) f a = Fix (TApp dummyPos f a)
infixl 1 :$

pattern Lambda :: Var -> Kind -> Type -> Type
pattern Lambda x k b <- Fix (TLambda _ x k b)
  where Lambda x k b = Fix (TLambda dummyPos x k b)

pattern Forall :: Var -> Kind -> Type -> Type
pattern Forall x k b <- Fix (TForall _ x k b)
  where Forall x k b = Fix (TForall dummyPos x k b)

pattern Exists :: Var -> Kind -> Type -> Type
pattern Exists x k b <- Fix (TExists _ x k b)
  where Exists x k b = Fix (TExists dummyPos x k b)

pattern Mu :: Var -> Type -> Type
pattern Mu x b <- Fix (TMu _ x b)
  where Mu x b = Fix (TMu dummyPos x b)

pattern Let :: Text -> [(Var, Kind)] -> Type -> Program a -> Program a
pattern Let name params ty cont <- Fix (Compose (Now (PLet _ (Definition _ (GlobalName name) params ty) cont)))
  where Let name params ty cont = Fix (Compose (Now (PLet dummyPos (Definition dummyPos (GlobalName name) params ty) cont)))

pattern Return :: Type -> Program a
pattern Return ty <- Fix (Compose (Now (PReturn _ ty)))
  where Return ty = Fix (Compose (Now (PReturn dummyPos ty)))
