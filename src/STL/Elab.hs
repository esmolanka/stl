{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STL.Elab where

import Control.Monad.Reader

import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Foldable (Fix(..), cata)
import Data.Functor.Identity
import Data.Maybe

import STL.Syntax.Position
import STL.Syntax.Types

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

dsFunctorBindings :: Maybe (Binding ()) -> (Core.Var, Core.Kind)
dsFunctorBindings Nothing = (Core.Var "ϕ", Core.Arr Core.Star Core.Covariant Core.Star)
dsFunctorBindings (Just (Binding p x k _)) = (dsVar x, maybe (Core.Arr Core.Star Core.Covariant Core.Star) (dsKind p) k)

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
      TRecord pos mixin mf -> do
        let rho = Core.Var "ρ"
        mixin' <- mixin
        mf' <- sequence mf
        pure $ Fix $ Core.TExists pos rho Core.Row
             $ Core.unspine (Fix $ Core.TRecord pos)
                 [ Core.unspine mixin'
                     [ fromMaybe (identityFunctor pos) mf'
                     , Fix $ Core.TRef pos rho 0
                     ]
                 ]
      TVariant pos mixin mf -> do
        let rho = Core.Var "ρ"
        mixin' <- mixin
        mf' <- sequence mf
        pure $ Fix $ Core.TForall pos rho Core.Row
             $ Core.unspine (Fix $ Core.TVariant pos)
                 [ Core.unspine mixin'
                     [ fromMaybe (identityFunctor pos) mf'
                     , Fix $ Core.TRef pos rho 0
                     ]
                 ]
      TMixin pos mbnd row -> do
        let rho = Core.Var "ρ"
            (phi, phiKind) = dsFunctorBindings mbnd
        ty <- fmap (dsRow (Fix $ Core.TRef pos rho 0)) . forM row $ \ty -> do
          ty' <- ty
          pure $ if isJust mbnd
            then ty'
            else Core.unspine (Fix (Core.TRef pos phi 0)) [ty']
        pure $ Fix $ Core.TLambda pos phi phiKind Core.Covariant
             $ Fix $ Core.TLambda pos rho Core.Row Core.Covariant
             $ ty
      TUnion pos a b cs -> do
        let phi = Core.Var "ϕ"
            rho = Core.Var "ρ"
        a' <- a
        b' <- b
        cs' <- sequence cs
        let union x y = Core.unspine x [Fix (Core.TRef pos phi 0), y]
        pure $ Fix $ Core.TLambda pos phi (Core.Arr Core.Star Core.Covariant Core.Star) Core.Covariant
             $ Fix $ Core.TLambda pos rho Core.Row Core.Covariant
             $ foldr union (Fix (Core.TRef pos rho 0)) (a' : b' : cs')
      TTuple pos a bs -> do
        let rho = Core.Var "ρ"
        a' <- a
        bs' <- sequence bs
        pure $ Fix $ Core.TExists pos rho Core.Star $
          foldr
          (\x rest -> Core.unspine (Fix (Core.TPair pos)) [x, rest])
          (Fix (Core.TRef pos rho 0))
          (a' : bs')
      TArray pos a n -> do
        a' <- a
        n' <- n
        pure $ Core.unspine (Fix (Core.TArray pos)) [a', n']

    identityFunctor :: Position -> Core.Type
    identityFunctor pos =
      Fix $ Core.TLambda pos (Core.Var "x") Core.Star Core.Covariant
          $ Fix $ Core.TRef pos (Core.Var "x") 0


dsRow :: Core.Type -> Row Core.Type -> Core.Type
dsRow tail = go
  where
    omega = Core.Var "ω"

    go :: Row Core.Type -> Core.Type
    go = \case
      RNil _ -> tail
      RExtend pos lbl prs ty cont ->
        let prs' = case prs of
                     PPresent pos' -> Fix $ Core.TPresent pos'
                     PVariable pos' -> Fix $ Core.TExists pos' omega Core.Presence
                                           $ Fix $ Core.TRef pos' omega 0
        in Core.unspine
             (Fix (Core.TExtend pos (dsLabel lbl)))
             [ prs', ty, go cont]


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
