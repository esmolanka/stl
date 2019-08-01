{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Eval where

import Control.Monad.Reader

import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Foldable (Fix(..), cata)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Any (..))
import Data.Set (Set)
import qualified Data.Set as S

import STL.Types

----------------------------------------------------------------------
-- Substitutions

freeMeta :: MetaVar -> Type -> Bool
freeMeta name = getAny . cata alg
  where
    alg :: TypeF Any -> Any
    alg = \case
      TMeta _ n _ | n == name -> Any True
      other -> fold other

freeVars :: Type -> Set (Var, Int)
freeVars e = runReader (cata alg e) M.empty
  where
    push :: Var -> Reader (Map Var Int) a -> Reader (Map Var Int) a
    push x = local (M.alter (maybe (Just 1) (Just . succ)) x)

    alg :: TypeF (Reader (Map Var Int) (Set (Var, Int))) -> Reader (Map Var Int) (Set (Var, Int))
    alg = \case
      TRef _ x n -> do
        n' <- asks (M.findWithDefault 0 x)
        pure $ if n >= n' then S.singleton (x, n - n') else S.empty
      TLambda _ x _ b -> push x b
      TForall _ x _ b -> push x b
      TExists _ x _ b -> push x b
      TMu _ x b -> push x b
      other -> fold <$> sequence other

shift :: Int -> Var -> Type -> Type
shift d x e = runReader (cata alg e) 0
  where
    alg :: TypeF (Reader Int Type) -> Reader Int Type
    alg = \case
      TRef pos x' n -> do
        c <- ask
        return $ Fix $ TRef pos x' $
          if x == x' && n >= c then n + d else n
      TLambda pos x' k b -> do
        b' <- if x == x' then local succ b else b
        return $ Fix $ TLambda pos x' k b'
      TForall pos x' k b -> do
        b' <- if x == x' then local succ b else b
        return $ Fix $ TForall pos x' k b'
      TExists pos x' k b -> do
        b' <- if x == x' then local succ b else b
        return $ Fix $ TExists pos x' k b'
      TMu pos x' b -> do
        b' <- if x == x' then local succ b else b
        return $ Fix $ TMu pos x' b'
      other -> Fix <$> sequence other

subst :: Var -> Int -> Type -> Type -> Type
subst x n0 sub0 expr = runReader (cata alg expr) (n0, sub0)
  where
    succIndex :: Reader (Int, Type) a -> Reader (Int, Type) a
    succIndex = local (first succ)

    shifted :: Var -> Reader (Int, Type) a -> Reader (Int, Type) a
    shifted x' = local (second (shift 1 x'))

    alg :: TypeF (Reader (Int, Type) Type) -> Reader (Int, Type) Type
    alg = \case
      TRef pos x' n' -> do
        (n, sub) <- ask
        if x' == x && n' == n
          then return sub
          else return (Fix (TRef pos x' n'))
      TLambda pos x' k b -> do
        b' <- shifted x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (TLambda pos x' k b'))
      TForall pos x' k b -> do
        b' <- shifted x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (TForall pos x' k b'))
      TExists pos x' k b -> do
        b' <- shifted x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (TExists pos x' k b'))
      TMu pos x' b -> do
        b' <- shifted x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (TMu pos x' b'))
      other -> Fix <$> sequence other

substGlobal :: GlobalName -> (Position -> Type) -> Type -> Type
substGlobal name sub0 expr = runReader (cata alg expr) sub0
  where
    shifted :: Var -> Reader (Position -> Type) a -> Reader (Position -> Type) a
    shifted x' = local (\f pos -> shift 1 x' (f pos))

    alg :: TypeF (Reader (Position -> Type) Type) -> Reader (Position -> Type) Type
    alg = \case
      TGlobal pos name' -> do
        sub <- ask
        if name' == name
          then return (sub pos)
          else return (Fix (TGlobal pos name'))
      TLambda pos x' k b -> do
        b' <- shifted x' b
        return (Fix (TLambda pos x' k b'))
      TForall pos x' k b -> do
        b' <- shifted x' b
        return (Fix (TForall pos x' k b'))
      TExists pos x' k b -> do
        b' <- shifted x' b
        return (Fix (TExists pos x' k b'))
      TMu pos x' b -> do
        b' <- shifted x' b
        return (Fix (TMu pos x' b'))
      other -> Fix <$> sequence other

normalise :: forall m. (Monad m) => (GlobalName -> m (Maybe (Type, Kind))) -> Type -> m Type
normalise resolveGlobal = cata alg
  where
    alg :: TypeF (m Type) -> m Type
    alg = \case
      TGlobal pos name -> do
        definition <- resolveGlobal name
        case definition of
          Nothing -> pure (Fix (TGlobal pos name))
          Just (term, _) -> pure term
      TApp pos f a -> do
        f' <- f
        a' <- a
        case f' of
          Fix (TLambda _ x _ b') ->
            normalise resolveGlobal $ shift (-1) x $ subst x 0 (shift 1 x a') b'
          _other ->
            pure (Fix (TApp pos f' a'))
      other ->
        Fix <$> sequence other

normaliseClosed :: Type -> Type
normaliseClosed t =
  runReader (normalise (const (pure Nothing)) t) ()
