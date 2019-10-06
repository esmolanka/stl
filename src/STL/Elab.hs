{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STL.Elab where

import Control.Arrow (first, second)

import Control.Monad.Reader
import Control.Monad.State

import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Foldable (Fix(..), cata)
import Data.Functor.Identity
import qualified Data.Set as S
import qualified Data.Text as T

import STL.Syntax.Position
import STL.Syntax.Types

import qualified STL.Core.Eval as Core
import qualified STL.Core.Types as Core
import STL.Pretty

----------------------------------------------------------------------
-- Desugaring / representation conversion

dsVariance :: Variance -> Core.Variance
dsVariance = \case
  Covariant -> Core.Covariant
  Contravariant -> Core.Contravariant
  Invariant -> Core.Invariant

dsVar :: Var -> Core.Var
dsVar = coerce

dsGlobalName :: GlobalName -> Core.GlobalName
dsGlobalName = coerce

dsLabel :: Label -> Core.Label
dsLabel = coerce

dsKind :: Position -> Kind -> Core.Kind
dsKind pos = \case
  Star -> Core.Star
  Nat -> Core.Nat
  Row -> Core.Row
  Arr a v b -> Core.Arr (dsKind pos a) (dsVariance v) (dsKind pos b)

dsBindings :: [Binding Variance] -> [(Core.Var, Core.Kind, Core.Variance)]
dsBindings = map (\(Binding p x k v) -> (dsVar x, maybe Core.Star (dsKind p) k, dsVariance v))

----------------------------------------------------------------------
-- Elaboration monad

data ElabContext a = ElabContext
  { ectxHandlers     :: Handlers a
  }

type MonadElab a m = (MonadReader (ElabContext a) m)

data Handlers a = Handlers
  { hNormalise :: Position -> Core.Type -> a
  , hCheck     :: Position -> Core.Type -> Core.Type -> a
  , hImport    :: forall b. Position -> Core.Program b -> Core.Program a
  }

runElab :: Handlers a -> Reader (ElabContext a) b -> b
runElab handlers elab = runReader elab (ElabContext handlers)

----------------------------------------------------------------------
-- Elaboration procedures

elabType :: forall m a. (MonadElab a m) => Type -> m Core.Type
elabType sugared = pure $ runIdentity (cata alg sugared)
  where
    alg :: TypeF (Identity Core.Type) -> Identity Core.Type
    alg = \case
      T pos bt -> case bt of
        TUnit   -> pure $ Fix (Core.TBase pos Core.TUnit)
        TVoid   -> pure $ Fix (Core.TBase pos Core.TVoid)
        TBool   -> pure $ Fix (Core.TBase pos Core.TBool)
        TInt    -> pure $ Fix (Core.TBase pos Core.TInt)
        TFloat  -> pure $ Fix (Core.TBase pos Core.TFloat)
        TString -> pure $ Fix (Core.TBase pos Core.TString)
        TList   -> pure $ Fix (Core.TBase pos Core.TList)
        TDict   -> pure $ Fix (Core.TBase pos Core.TDict)
        TNat    -> pure $ Fix (Core.TBase pos Core.TNat)
      TRef pos x ->
        pure $ Fix (Core.TRef pos (dsVar x) 0)
      TGlobal pos Nothing name -> do
        pure $ Fix (Core.TGlobal pos (dsGlobalName name))
      TGlobal pos (Just _mod) _name ->
        error $ show $ pretty pos <> colon <+> "Qualified names are not yet supported"
      TForall _ bnds body -> do
        body' <- body
        pure $ foldr (\(Binding pos var k _) -> Fix . Core.TForall (pos <> Core.getPosition body') (dsVar var) (maybe Core.Star (dsKind pos) k)) body' bnds
      TExists _ bnds body -> do
        body' <- body
        pure $ foldr (\(Binding pos var k _) -> Fix . Core.TExists pos (dsVar var) (maybe Core.Star (dsKind pos) k)) body' bnds
      TArrow pos a b cs -> do
        a' <- a
        b' <- b
        cs' <- sequence cs
        pure $ foldr (\x rest -> Core.unspine (Fix (Core.TArrow pos)) [x, rest]) (last (a' : b' : cs')) (init (a' : b' : cs'))
      TApp _ f a as -> do
        f' <- f
        a' <- a
        as' <- sequence as
        pure $ Core.unspine f' (a' : as')
      TRecord pos row -> do
        (row', (pvars, rvars)) <- dsRow <$> sequence row
        let withP k = foldr (\x -> Fix . Core.TExists pos x Core.Presence) k pvars
            withR k = foldr (\x -> Fix . Core.TExists pos x Core.Row) k rvars
        pure $ withR $ withP $ Core.unspine (Fix (Core.TRecord pos)) [row']
      TVariant pos row -> do
        (row', (pvars, rvars)) <- dsRow <$> sequence row
        let withP k = foldr (\x -> Fix . Core.TExists pos x Core.Presence) k pvars
            withR k = foldr (\x -> Fix . Core.TForall pos x Core.Row) k rvars
        pure $ withR $ withP $ Core.unspine (Fix (Core.TVariant pos)) [row']
      TArray pos a n -> do
        a' <- a
        n' <- n
        pure $ Core.unspine (Fix (Core.TArray pos)) [a', n']

dsRow :: Row Core.Type -> (Core.Type, ([Core.Var], [Core.Var]))
dsRow t =
  let (ty, (ps, rs)) = runState (go t) (0, 0)
  in (ty, (replicate ps omega, replicate rs rho))
  where
    freeVars = S.map fst $ foldMap Core.freeVars t
    mkVar x =
      head $ filter (`S.notMember` freeVars) $
        map (\n -> Core.Var (x <> T.replicate n "\'")) [0..]

    omega = mkVar "ω"
    rho   = mkVar "ρ"

    freshPresence :: Position -> State (Int, Int) Core.Type
    freshPresence pos = do
      n <- gets fst
      modify (first succ)
      pure (Fix (Core.TRef pos omega n))

    freshRow :: Position -> State (Int, Int) Core.Type
    freshRow pos = do
      n <- gets snd
      modify (second succ)
      pure (Fix (Core.TRef pos rho n))

    go :: Row Core.Type -> State (Int, Int) Core.Type
    go = \case
      RNil pos -> freshRow pos
      RExplicit _ ty -> pure ty
      RExtend pos lbl prs ty cont -> do
        prs' <- case prs of
                  PPresent pos' -> pure (Fix (Core.TPresent pos'))
                  PVariable pos' -> freshPresence pos'
        cont' <- go cont
        pure $ Core.unspine (Fix (Core.TExtend pos (dsLabel lbl)))
          [ prs', ty, cont']


elabStatements :: forall m a. (MonadElab a m) => [Statement] -> m (Core.Program a) -> m (Core.Program a)
elabStatements = flip (foldr elabStatement)

elabStatement :: forall m a. (MonadElab a m) => Statement -> m (Core.Program a) -> m (Core.Program a)
elabStatement  stmt cont = do
  Handlers{hNormalise, hCheck} <- asks ectxHandlers
  case stmt of
    Typedef pos name params body -> do
      defn <-
        Core.Definition pos (dsGlobalName name) (dsBindings params) <$> elabType body
      cont' <- cont
      pure $ Fix (Compose (Core.Now (Core.PLet pos defn cont')))

    Mutualdef pos params clauses -> do
      defs <-
        forM clauses $ \(MutualClause clausePos name body) ->
          Core.Definition clausePos (dsGlobalName name) (dsBindings params) <$> elabType body
      cont' <- cont
      pure $ Fix (Compose (Core.Now (Core.PMutual pos defs cont')))

    Normalise pos ty -> do
      ty' <- elabType ty
      Fix (Compose kont) <- cont
      pure $ Fix (Compose (Core.Later (hNormalise pos ty') kont))

    Subsume pos sub super -> do
      sub' <- elabType sub
      super' <- elabType super
      Fix (Compose kont) <- cont
      pure $ Fix (Compose (Core.Later (hCheck pos sub' super') kont))

elabReturn :: forall m a. (MonadElab a m) => Maybe Type -> m (Core.Program a)
elabReturn ty = do
  leaf <- maybe (pure Core.PNil) (\t -> Core.PReturn (typePos t) <$> elabType t) ty
  pure $ Fix (Compose (Core.Now leaf))

elabModule :: forall m a. (MonadElab a m) => Module -> m (Core.Program a)
elabModule (Module _name _imports statements rettype) =
  elabStatements statements (elabReturn rettype)

----------------------------------------------------------------------

dsStatement :: Handlers a -> Statement -> Core.Program a -> Core.Program a
dsStatement handlers stmt cont =
  runElab handlers (elabStatement stmt (pure cont))

dsReturn :: Handlers a -> Maybe Type -> Core.Program a
dsReturn handlers ty =
  runElab handlers (elabReturn ty)

dsModule :: Handlers a -> Module -> Core.Program a
dsModule handlers modul =
  runElab handlers (elabModule modul)
