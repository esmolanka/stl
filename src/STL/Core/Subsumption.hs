{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Core.Subsumption
  ( subsumedBy
  , runSubsumption
  , UnifyErr (..)
  , UnifyState
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Foldable (Fix(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import STL.Core.Eval
import STL.Core.Types
import STL.Pretty as PP

----------------------------------------------------------------------
-- Unification

data UnifyErr
  = IsNotSubtypeOf         Type Type
  | ArgumentCountMismatch  Int  Int
  | InfiniteType           Type
  | CannotEscapeBindings   (Set (Var, Int))
  | NotPolymorphicEnough   Type Var
  | GlobalNotFound         GlobalName
  | UnreducedApplication   Type

instance CPretty UnifyErr where
  cpretty = \case
    IsNotSubtypeOf a b ->
      nest 2 $ vsep
        [ "Cannot unify types."
        , indent 2 $ cpretty a
        , "Is not subsumed by:"
        , indent 2 $ cpretty b
        ]
    ArgumentCountMismatch n m ->
      nest 2 $ vsep
        [ "Cannot unify type application, different number of arguments:"
        , indent 2 $ pretty n <+> "vs." <+> pretty m
        ]
    InfiniteType t ->
      nest 2 $ vsep
        [ "Infinite type:"
        , indent 2 $ cpretty t
        ]
    CannotEscapeBindings vars ->
      nest 2 $ vsep
        [ "Variables should not escape their scope:"
        , indent 2 $ PP.list $ map (\(x, n) -> cpretty (Fix (TRef dummyPos x n))) $ S.toList vars
        ]
    NotPolymorphicEnough ty var ->
      nest 2 $ vsep
        [ "Type is not polymorphic enough."
        , "Rigid variable" <+> cpretty var <+> "does not unify with:"
        , indent 2 $ cpretty ty
        ]
    GlobalNotFound name ->
      nest 2 $ vsep
        [ "Undefined type" <+> squotes (cpretty name) <> "."
        ]
    UnreducedApplication t ->
      nest 2 $ vsep
        [ "Unreduced type application:"
        , indent 2 $ cpretty t
        ]

data UnifyEnv = UnifyEnv
  { envVarMap  :: [(Var, Var)]
  , envGlobals  :: Map GlobalName Kind
  }

data UnifyState = UnifyState
  { stFreshName   :: Int
  , stMetas       :: IntMap (Position, Var, Type)
  , stAssumptions :: Set (Type, Type)
  }

instance CPretty UnifyState where
  cpretty st =
    PP.list $
      map (\(name, (_, hint, ty)) -> "?" <> cpretty hint <> brackets (pretty name) <+> "->" <+> cpretty ty) $
        IM.toList (stMetas st)

type MonadUnify m =
  ( MonadError UnifyErr m
  , MonadReader UnifyEnv m
  , MonadState UnifyState m
  )

pushAlphaEq :: (MonadUnify m) => Var -> Var -> m a -> m a
pushAlphaEq x x' =
  local (\env -> env { envVarMap = (x, x') : envVarMap env })

checkAlphaEq :: (MonadUnify m) => (Var, Int) -> (Var, Int) -> m Bool
checkAlphaEq (x, n) (y, m) = go 0 0 <$> asks envVarMap
  where
    go :: Int -> Int -> [(Var, Var)] -> Bool
    go !n' !m' = \case
      ((x', y') : rest) ->
        x == x' && y == y' && n == n' && m == m' ||
        go (if x == x' then succ n' else n')
           (if y == y' then succ m' else m')
           rest
      [] ->
        x == y && n' == m'

lookupGlobalVariances :: (MonadUnify m) => GlobalName -> m (Maybe [Variance])
lookupGlobalVariances name = asks (fmap getVariances . M.lookup name . envGlobals)
  where
    getVariances :: Kind -> [Variance]
    getVariances = \case
      Arr _ v rest -> v : getVariances rest
      _ -> []

newMeta :: (MonadUnify m) => Position -> Var -> Kind -> m Type
newMeta pos hint kind = do
  modify (\st -> st { stFreshName = succ (stFreshName st) })
  n <- gets stFreshName
  pure (Fix (TMeta pos (MetaVar n) hint kind))

newSkolem :: (MonadUnify m) => Position -> Var -> Kind -> m Type
newSkolem pos hint kind = do
  modify (\st -> st { stFreshName = succ (stFreshName st) })
  n <- gets stFreshName
  pure (Fix (TSkolem pos (Skolem n) hint kind))

lookupMeta :: (MonadUnify m) => MetaVar -> m (Maybe Type)
lookupMeta (MetaVar name) =
  fmap (\(_, _, t) -> t) <$> gets (IM.lookup name . stMetas)

writeMeta :: (MonadUnify m) => MetaVar -> Position -> Var -> Type -> m ()
writeMeta (MetaVar name) pos hint ty =
  modify (\st -> st { stMetas = IM.insert name (pos, hint, ty) (stMetas st) })

recordAssumption :: (MonadUnify m) => Type -> Type -> m ()
recordAssumption sub sup =
  modify (\st -> st { stAssumptions = S.insert (sub, sup) (stAssumptions st) })

isSeenAssumption :: (MonadUnify m) => Type -> Type -> m Bool
isSeenAssumption sub sup =
  gets (S.member (sub, sup) . stAssumptions)

subsumedBy :: forall m. (MonadUnify m) => Type -> Type -> m ()
subsumedBy sub sup = do
  seen <- isSeenAssumption sub sup
  if seen
    then
      -- If the assumption is already seen and the unification did not
      -- fail, then the assumption was correct. Therefore, nothing
      -- left to do.
      pure ()
    else
      -- First assume that these types unify for the sake of Mu-types
      -- unification and then check if it is the case.
      recordAssumption sub sup >> checkAssumption
  where
    (Fix tyConSub, argsSub) = spine sub
    (Fix tyConSup, argsSup) = spine sup

    checkAssumption :: m ()
    checkAssumption =
      case (tyConSub, argsSub, tyConSup, argsSup) of
        (TForall pos x k b, _, _, _) -> do
          imp <- newMeta pos x k
          unspine (subst x 0 imp b) argsSub `subsumedBy` sup

        (_, _, TForall pos' x' k' b', []) -> do
          imp <- newSkolem pos' x' k'
          sub `subsumedBy` unspine (subst x' 0 imp b') argsSup

        (TExists pos x k b, _, _, _) -> do
          imp <- newSkolem pos x k
          unspine (subst x 0 imp b) argsSub `subsumedBy` sup

        (_, _, TExists pos' x' k' b', _) -> do
          imp <- newMeta pos' x' k'
          sub `subsumedBy` unspine (subst x' 0 imp b') argsSup

        (TMu _ x b, _, _, _) ->
          unspine (shift (-1) x (subst x 0 (shift 1 x sub) b)) argsSub `subsumedBy` sup

        (_, _, TMu _ x' b', []) ->
          sub `subsumedBy` unspine (shift (-1) x' (subst x' 0 (shift 1 x' sup) b')) argsSup

        (TMeta pos n hint _, [], _, _) -> do
          subsumedByMeta MetaToType pos hint n sup

        (_, _, TMeta pos' n' hint' _, []) -> do
          subsumedByMeta TypeToMeta pos' hint' n' sub

        (TLambda _ x k v b, _, TLambda _ x' k' v' b', _) -> do
          unless (k == k') $
            throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
          unless (v == v') $
            throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
          unless (null argsSub) $
            throwError $ UnreducedApplication sub
          unless (null argsSup) $
            throwError $ UnreducedApplication sup
          pushAlphaEq x x' $
            subsumedBy b b'

        (TRef _ x n, _, TRef _ x' n', _) -> do
          mathces <- checkAlphaEq (x, n) (x', n')
          unless mathces $
            throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
          subsumedBySpine (map (const Covariant) argsSup) argsSub argsSup

        (TGlobal _ n, _, TGlobal _ n', _) | n == n' -> do
          mvs <- lookupGlobalVariances n
          case mvs of
            Nothing -> throwError $ GlobalNotFound n
            Just vs -> subsumedBySpine vs argsSub argsSup

        (TArrow _, [a, b], TArrow _, [a', b']) ->
          a' `subsumedBy` a >>  -- Argument is contravariant
          b  `subsumedBy` b'    -- Return type is covariant

        (TNil _, [], TNil _, []) ->
          pure ()

        (TNil pos, [], TExtend _ lbl', [_, f', _]) ->
          unspine (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
          `subsumedBy`
          sup

        (TExtend _ lbl, [_, f, _], TNil pos', []) ->
          sub
          `subsumedBy`
          unspine (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

        (TSkolem pos _ _ _, [], TExtend _ lbl', [_, f', _]) ->
          unspine (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
          `subsumedBy`
          sup

        (TExtend _ lbl, [_, f, _], TSkolem pos' _ _ _, []) ->
          sub
          `subsumedBy`
          unspine (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

        (TExtend _ lbl, [pty, fty, tail_], TExtend pos' lbl', [pty', fty', tail']) -> do
          (pty'', fty'', tail'') <- rewriteRow TypeToMeta lbl pos' lbl' pty' fty' tail'
          subsumedBySpine [Covariant, Covariant, Covariant] [pty, fty, tail_] [pty'', fty'', tail'']

        (TSkolem _ n _ _, [], TSkolem _ n' _ _, [])
          | n == n' -> pure ()

        (TSkolem _ _ var _, [], _, _) ->
          throwError $ NotPolymorphicEnough sup var

        (_, _, TSkolem _ _ var' _, []) ->
          throwError $ NotPolymorphicEnough sub var'

        _ | tyConSub `eqTypeCon` tyConSup ->
              subsumedBySpine (map (const Covariant) argsSub) argsSub argsSup
          | otherwise ->
              throwError $ IsNotSubtypeOf sub sup

data Direction = MetaToType | TypeToMeta

subsumedByMeta :: forall m. (MonadUnify m) => Direction -> Position -> Var -> MetaVar -> Type -> m ()
subsumedByMeta dir pos hint n other = do
  mty <- lookupMeta n
  case mty of
    Nothing ->
      if freeMeta n other
      then throwError $ InfiniteType other
      else do
        let vars = freeVars other
        unless (S.null vars) $
          throwError $ CannotEscapeBindings vars
        writeMeta n pos hint other
    Just ty ->
      case dir of
        MetaToType -> ty `subsumedBy` other
        TypeToMeta -> other `subsumedBy` ty

rewriteRow :: (MonadUnify m) => Direction -> Label -> Position -> Label -> Type -> Type -> Type -> m (Type, Type, Type)
rewriteRow dir newLabel pos label pty fty tail_
  | newLabel == label = return (pty, fty, tail_)
  | otherwise =
      case spine tail_ of
        (Fix (TMeta pos' alpha _ _), []) -> do
          beta <- newMeta pos "β" Row
          gamma <- newMeta pos "γ" Star
          theta <- newMeta pos "θ" Presence
          subsumedByMeta dir pos "α" alpha (unspine (Fix (TExtend pos' newLabel)) [theta, gamma, beta])
          return (theta, gamma, unspine (Fix (TExtend pos label)) [pty, fty, beta])
        (Fix (TExtend pos' label'), [pty', fty', tail']) -> do
          (pty'', fty'', tail'') <- rewriteRow dir newLabel pos' label' pty' fty' tail'
          return (pty'', fty'', unspine (Fix (TExtend pos label)) [pty, fty, tail''])
        (Fix (TNil pos'), []) -> do
          gamma <- newMeta pos' "γ" Star
          return (Fix (TAbsent pos'), gamma, unspine (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        (Fix (TSkolem pos' _ _ _), []) -> do
          gamma <- newMeta pos' "γ" Star
          return (Fix (TAbsent pos'), gamma, unspine (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        _other ->
          error $ "Unexpected type: " ++ show tail_

subsumedByVariance :: forall m. (MonadUnify m) => Variance -> Type -> Type -> m ()
subsumedByVariance v sub sup =
  case v of
    Covariant -> sub `subsumedBy` sup
    Contravariant -> sup `subsumedBy` sub
    Invariant -> sub `subsumedBy` sup >> sup `subsumedBy` sub

subsumedBySpine :: forall m. (MonadUnify m) => [Variance] -> [Type] -> [Type] -> m ()
subsumedBySpine vs subs sups = do
  let countSubs = length subs
      countSups = length sups
  unless (countSubs == countSups) $
    throwError $ ArgumentCountMismatch countSubs countSups
  sequence_ $ zipWith3 subsumedByVariance (vs ++ repeat Invariant) subs sups

eqTypeCon :: TypeF Type -> TypeF Type -> Bool
eqTypeCon a b =
  case (a, b) of
    (TBase _ ta      , TBase _ tb      ) -> ta == tb
    (TArrow _        , TArrow _        ) -> True
    (TRecord _       , TRecord _       ) -> True
    (TVariant _      , TVariant _      ) -> True
    (TArray _        , TArray _        ) -> True
    (TPresent _      , TPresent _      ) -> True
    (TAbsent _       , TAbsent _       ) -> True
    (_               , _               ) -> False

runSubsumption :: Map GlobalName Kind -> ExceptT e (StateT UnifyState (Reader UnifyEnv)) a -> (Either e a, UnifyState)
runSubsumption globals k =
  runReader (runStateT (runExceptT k) initState) initEnv
  where
    initState = UnifyState { stFreshName = 100, stMetas = mempty, stAssumptions = mempty }
    initEnv   = UnifyEnv [] globals
