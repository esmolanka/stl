{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Subsumption where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor.Foldable (Fix(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc as PP
  ( Pretty(..), (<+>), vsep, nest, indent, list)

import STL.Types
import STL.Eval

----------------------------------------------------------------------
-- Unification

data UnifyErr
  = IsNotSubtypeOf Type Type
  | ArgumentCountMismatch Int Int
  | InfiniteType Type
  | CannotEscapeBindings (Set (Var, Int))
  | NotPolymorphicEnough Type Var

instance Pretty UnifyErr where
  pretty = \case
    IsNotSubtypeOf a b ->
      nest 4 $ vsep
        [ "Cannot unify types."
        , indent 4 (pretty a)
        , "is not a subtype of"
        , indent 4 (pretty b)
        ]
    ArgumentCountMismatch n m ->
      nest 4 $ vsep
        [ "Cannot unify type application, different number of arguments:"
        , pretty n <+> "vs." <+> pretty m
        ]
    InfiniteType t ->
      nest 4 $ vsep
        [ "Infinite type:"
        , pretty t
        ]
    CannotEscapeBindings vars ->
      nest 4 $ vsep
        [ "Variables should not escape their scope:"
        , PP.list $ map (\(x, n) -> pretty (Fix (TRef dummyPos x n))) $ S.toList vars
        ]
    NotPolymorphicEnough ty var ->
      nest 4 $ vsep
        [ "Type is not polymorphic enough:"
        , pretty var <+> "~" <+> pretty ty
        ]

data UnifyEnv = UnifyEnv
  { envVarMap :: [(Var, Var)]
  }

data UnifyState = UnifyState
  { stFreshName :: Int
  , stMetas :: IntMap Type
  }

instance Pretty UnifyState where
  pretty st =
    PP.list $
      map (\(name, ty) -> "?" <> pretty name <+> "->" <+> pretty ty) $
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

newMeta :: (MonadUnify m) => Position -> Kind -> m Type
newMeta pos kind = do
  modify (\st -> st { stFreshName = succ (stFreshName st) })
  n <- gets stFreshName
  pure (Fix (TMeta pos (MetaVar n) kind))

newSkolem :: (MonadUnify m) => Position -> Var -> Kind -> m Type
newSkolem pos hint kind = do
  modify (\st -> st { stFreshName = succ (stFreshName st) })
  n <- gets stFreshName
  pure (Fix (TSkolem pos (Skolem n) hint kind))

lookupMeta :: (MonadUnify m) => MetaVar -> m (Maybe Type)
lookupMeta (MetaVar name) =
  gets (IM.lookup name . stMetas)

writeMeta :: (MonadUnify m) => MetaVar -> Type -> m ()
writeMeta (MetaVar name) ty =
  modify (\st -> st { stMetas = IM.insert name ty (stMetas st) })

subsumes :: forall m. (MonadUnify m) => Type -> Type -> m ()
subsumes sub sup =
  let (Fix tyConSub, argsSub) = tele sub
      (Fix tyConSup, argsSup) = tele sup
  in case (tyConSub, argsSub, tyConSup, argsSup) of
       (TForall pos x k b, [], _, _) -> do
         imp <- newMeta pos k
         subst x 0 imp b `subsumes` sup

       (_, _, TForall pos' x' k' b', []) -> do
         imp <- newSkolem pos' x' k'
         sub `subsumes` subst x' 0 imp b'

       (TMeta _ n _, [], _, _) -> do
         subsumesMeta MetaToType n sup

       (_, _, TMeta _ n' _, []) -> do
         subsumesMeta TypeToMeta n' sub

       (TRef _ x n, _, TRef _ x' n', _) -> do
         mathces <- checkAlphaEq (x, n) (x', n')
         unless mathces $
           throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
         argsSub `subsumesTele` argsSup

       (TGlobal _ n, _, TGlobal _ n', _)
         | n == n' -> argsSub `subsumesTele` argsSup

       (TArrow _, (a : bs), TArrow _, (a' : bs')) ->
         a' `subsumes` a >>    -- Argument is contravariant
         bs `subsumesTele` bs' -- Return type is covariant

       (TLambda _ x k b, _, TLambda _ x' k' b', _) -> do
         unless (k == k') $
           throwError $ IsNotSubtypeOf (Fix tyConSub) (Fix tyConSup)
         pushAlphaEq x x' $
           subsumes b b'
         argsSub `subsumesTele` argsSup

       (TMu _ x b, [], TMu _ x' b', []) -> do
         pushAlphaEq x x' $
           subsumes b b'

       (TNil _, [], TNil _, []) ->
         pure ()

       (TNil pos, [], TExtend _ lbl', [_, f', _]) ->
         untele (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
         `subsumes`
         sup

       (TExtend _ lbl, [_, f, _], TNil pos', []) ->
         sub
         `subsumes`
         untele (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

       (TSkolem pos _ _ _, [], TExtend _ lbl', [_, f', _]) ->
         untele (Fix (TExtend pos lbl')) [Fix (TAbsent pos), f', Fix (TNil pos)]
         `subsumes`
         sup

       (TExtend _ lbl, [_, f, _], TSkolem pos' _ _ _, []) ->
         sub
         `subsumes`
         untele (Fix (TExtend pos' lbl)) [Fix (TAbsent pos'), f, Fix (TNil pos')]

       (TExtend _ lbl, [pty, fty, tail_], TExtend pos' lbl', [pty', fty', tail']) -> do
         (pty'', fty'', tail'') <- rewriteRow TypeToMeta lbl pos' lbl' pty' fty' tail'
         [pty, fty, tail_] `subsumesTele` [pty'', fty'', tail'']

       (TSkolem _ n _ _, [], TSkolem _ n' _ _, [])
         | n == n' -> pure ()

       (TSkolem _ _ var _, [], _, _) ->
         throwError $ NotPolymorphicEnough sup var

       (_, _, TSkolem _ _ var' _, []) ->
         throwError $ NotPolymorphicEnough sub var'

       _ | tyConSub `eqTypeCon` tyConSup ->
             subsumesTele argsSub argsSup
         | otherwise ->
             throwError $ IsNotSubtypeOf sub sup

data Direction = MetaToType | TypeToMeta

subsumesMeta :: forall m. (MonadUnify m) => Direction -> MetaVar -> Type -> m ()
subsumesMeta dir n other = do
  mty <- lookupMeta n
  case mty of
    Nothing ->
      if freeMeta n other
      then throwError $ InfiniteType other
      else do
        let vars = freeVars other
        unless (S.null vars) $
          throwError $ CannotEscapeBindings vars
        writeMeta n other
    Just ty ->
      case dir of
        MetaToType -> ty `subsumes` other
        TypeToMeta -> other `subsumes` ty

rewriteRow :: (MonadUnify m) => Direction -> Label -> Position -> Label -> Type -> Type -> Type -> m (Type, Type, Type)
rewriteRow dir newLabel pos label pty fty tail_
  | newLabel == label = return (pty, fty, tail_)
  | otherwise =
      case tele tail_ of
        (Fix (TMeta pos' alpha _), []) -> do
          beta <- newMeta pos Row
          gamma <- newMeta pos Star
          theta <- newMeta pos Presence
          subsumesMeta dir alpha (untele (Fix (TExtend pos' newLabel)) [theta, gamma, beta])
          return (theta, gamma, untele (Fix (TExtend pos label)) [pty, fty, beta])
        (Fix (TExtend pos' label'), [pty', fty', tail']) -> do
          (pty'', fty'', tail'') <- rewriteRow dir newLabel pos' label' pty' fty' tail'
          return (pty'', fty'', untele (Fix (TExtend pos label)) [pty, fty, tail''])
        (Fix (TNil pos'), []) -> do
          gamma <- newMeta pos' Star
          return (Fix (TAbsent pos'), gamma, untele (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        (Fix (TSkolem pos' _ _ _), []) -> do
          gamma <- newMeta pos' Star
          return (Fix (TAbsent pos'), gamma, untele (Fix (TExtend pos label)) [pty, fty, Fix (TNil pos')])
        _other ->
          error $ "Unexpected type: " ++ show tail_

subsumesTele :: forall m. (MonadUnify m) => [Type] -> [Type] -> m ()
subsumesTele subs sups = do
  let countSubs = length subs
      countSups = length sups
  unless (countSubs == countSups) $
    throwError $ ArgumentCountMismatch countSubs countSups
  zipWithM_ subsumes subs sups

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
    (TMeta _ _ _     , TMeta _ _ _     ) -> False
    (TLambda _ _ _ _ , TLambda _ _ _ _ ) -> False
    (TForall _ _ _ _ , TForall _ _ _ _ ) -> False
    (TMu _ _ _       , TMu _ _ _       ) -> False
    (TApp _ _ _      , TApp _ _ _      ) -> False
    (_               , _               ) -> False

runSubsumption :: ExceptT e (StateT UnifyState (Reader UnifyEnv)) a -> (Either e a, UnifyState)
runSubsumption k =
  runReader (runStateT (runExceptT k) initState) initEnv
  where
    initState = UnifyState { stFreshName = 100, stMetas = mempty }
    initEnv   = UnifyEnv []
