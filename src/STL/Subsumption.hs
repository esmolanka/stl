{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Subsumption where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Foldable (Fix(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S

import STL.Eval
import STL.Pretty as PP
import STL.Types

----------------------------------------------------------------------
-- Unification

data UnifyErr
  = IsNotSubtypeOf Type Type
  | ArgumentCountMismatch Int Int
  | InfiniteType Type
  | CannotEscapeBindings (Set (Var, Int))
  | NotPolymorphicEnough Type Var

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

data UnifyEnv = UnifyEnv
  { envVarMap :: [(Var, Var)]
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
    (Fix tyConSub, argsSub) = tele sub
    (Fix tyConSup, argsSup) = tele sup

    checkAssumption :: m ()
    checkAssumption =
      case (tyConSub, argsSub, tyConSup, argsSup) of
        (TForall pos x k b, [], _, _) -> do
          imp <- newMeta pos x k
          subst x 0 imp b `subsumedBy` sup

        (_, _, TForall pos' x' k' b', []) -> do
          imp <- newSkolem pos' x' k'
          sub `subsumedBy` subst x' 0 imp b'

        (TExists pos x k b, [], _, _) -> do
          imp <- newSkolem pos x k
          subst x 0 imp b `subsumedBy` sup

        (_, _, TExists pos' x' k' b', []) -> do
          imp <- newMeta pos' x' k'
          sub `subsumedBy` subst x' 0 imp b'

        (TMu _ x b, [], _, _) ->
          shift (-1) x (subst x 0 (shift 1 x sub) b) `subsumedBy` sup

        (_, _, TMu _ x' b', []) ->
          sub `subsumedBy` shift (-1) x' (subst x' 0 (shift 1 x' sup) b')

        (TMeta pos n hint _, [], _, _) -> do
          subsumedByMeta MetaToType pos hint n sup

        (_, _, TMeta pos' n' hint' _, []) -> do
          subsumedByMeta TypeToMeta pos' hint' n' sub

        (TLambda _ x k b, _, TLambda _ x' k' b', _) -> do
          unless (k == k') $
            throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
          pushAlphaEq x x' $
            subsumedBy b b'
          argsSub `subsumedByTele` argsSup

        (TRef _ x n, _, TRef _ x' n', _) -> do
          mathces <- checkAlphaEq (x, n) (x', n')
          unless mathces $
            throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
          argsSub `subsumedByTele` argsSup

        (TGlobal _ n, _, TGlobal _ n', _) | n == n' ->
          -- Type definitions' parameters are checked by the kind
          -- checker to be strictly positive, therefore can be treated
          -- as covariant.
          argsSub `subsumedByTele` argsSup

        (TArrow _, [a, b], TArrow _, [a', b']) ->
          a' `subsumedBy` a >>  -- Argument is contravariant
          b  `subsumedBy` b'    -- Return type is covariant

        (TNil _, [], TNil _, []) ->
          pure ()

        (TNil pos, [], TExtend _ lbl', [_, f', _]) ->
          untele (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
          `subsumedBy`
          sup

        (TExtend _ lbl, [_, f, _], TNil pos', []) ->
          sub
          `subsumedBy`
          untele (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

        (TSkolem pos _ _ _, [], TExtend _ lbl', [_, f', _]) ->
          untele (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
          `subsumedBy`
          sup

        (TExtend _ lbl, [_, f, _], TSkolem pos' _ _ _, []) ->
          sub
          `subsumedBy`
          untele (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

        (TExtend _ lbl, [pty, fty, tail_], TExtend pos' lbl', [pty', fty', tail']) -> do
          (pty'', fty'', tail'') <- rewriteRow TypeToMeta lbl pos' lbl' pty' fty' tail'
          [pty, fty, tail_] `subsumedByTele` [pty'', fty'', tail'']

        (TSkolem _ n _ _, [], TSkolem _ n' _ _, [])
          | n == n' -> pure ()

        (TSkolem _ _ var _, [], _, _) ->
          throwError $ NotPolymorphicEnough sup var

        (_, _, TSkolem _ _ var' _, []) ->
          throwError $ NotPolymorphicEnough sub var'

        _ | tyConSub `eqTypeCon` tyConSup ->
              subsumedByTele argsSub argsSup
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
      case tele tail_ of
        (Fix (TMeta pos' alpha _ _), []) -> do
          beta <- newMeta pos "β" Row
          gamma <- newMeta pos "γ" Star
          theta <- newMeta pos "θ" Presence
          subsumedByMeta dir pos "α" alpha (untele (Fix (TExtend pos' newLabel)) [theta, gamma, beta])
          return (theta, gamma, untele (Fix (TExtend pos label)) [pty, fty, beta])
        (Fix (TExtend pos' label'), [pty', fty', tail']) -> do
          (pty'', fty'', tail'') <- rewriteRow dir newLabel pos' label' pty' fty' tail'
          return (pty'', fty'', untele (Fix (TExtend pos label)) [pty, fty, tail''])
        (Fix (TNil pos'), []) -> do
          gamma <- newMeta pos' "γ" Star
          return (Fix (TAbsent pos'), gamma, untele (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        (Fix (TSkolem pos' _ _ _), []) -> do
          gamma <- newMeta pos' "γ" Star
          return (Fix (TAbsent pos'), gamma, untele (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        _other ->
          error $ "Unexpected type: " ++ show tail_

subsumedByTele :: forall m. (MonadUnify m) => [Type] -> [Type] -> m ()
subsumedByTele subs sups = do
  let countSubs = length subs
      countSups = length sups
  unless (countSubs == countSups) $
    throwError $ ArgumentCountMismatch countSubs countSups
  zipWithM_ subsumedBy subs sups

eqTypeCon :: TypeF Type -> TypeF Type -> Bool
eqTypeCon a b =
  case (a, b) of
    -- Trivial
    (TUnit _         , TUnit _         ) -> True
    (TVoid _         , TVoid _         ) -> True
    (TArrow _        , TArrow _        ) -> True
    (TRecord _       , TRecord _       ) -> True
    (TVariant _      , TVariant _      ) -> True
    (TPresent _      , TPresent _      ) -> True
    (TAbsent _       , TAbsent _       ) -> True

    (TExtend _ la    , TExtend _ lb    ) -> la == lb
    (TNil _          , TNil _          ) -> True

    -- Non-trivial, therefore never equal
    (TRef _ _ _      , TRef _ _ _      ) -> False
    (TMeta _ _ _ _   , TMeta _ _ _ _   ) -> False
    (TLambda _ _ _ _ , TLambda _ _ _ _ ) -> False
    (TForall _ _ _ _ , TForall _ _ _ _ ) -> False
    (TMu _ _ _       , TMu _ _ _       ) -> False
    (TApp _ _ _      , TApp _ _ _      ) -> False
    (_               , _               ) -> False

runSubsumption :: ExceptT e (StateT UnifyState (Reader UnifyEnv)) a -> (Either e a, UnifyState)
runSubsumption k =
  runReader (runStateT (runExceptT k) initState) initEnv
  where
    initState = UnifyState { stFreshName = 100, stMetas = mempty, stAssumptions = mempty }
    initEnv   = UnifyEnv []
