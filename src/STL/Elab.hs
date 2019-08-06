{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STL.Elab where

import Control.Arrow (first, second)

import Control.Monad.State
import Data.Functor.Compose
import Data.Functor.Foldable (Fix(..), cata)
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Set as S

import STL.Syntax.Position
import STL.Syntax.Types
import qualified STL.Eval as Core
import qualified STL.Types as Core
import STL.Pretty

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
  Arr a b -> Core.Arr (dsKind pos a) (dsKind pos b)

dsBindings :: [Binding] -> [(Core.Var, Core.Kind)]
dsBindings = map (\(Binding p x k) -> (dsVar x, maybe Core.Star (dsKind p) k))

dsType :: Type -> Core.Type
dsType = cata alg
  where
    alg :: TypeF Core.Type -> Core.Type
    alg = \case
      T pos bt -> case bt of
        TUnit   -> Fix (Core.TBase pos Core.TUnit)
        TVoid   -> Fix (Core.TBase pos Core.TVoid)
        TBool   -> Fix (Core.TBase pos Core.TBool)
        TInt    -> Fix (Core.TBase pos Core.TInt)
        TFloat  -> Fix (Core.TBase pos Core.TFloat)
        TString -> Fix (Core.TBase pos Core.TString)
        TList   -> Fix (Core.TBase pos Core.TList)
        TDict   -> Fix (Core.TBase pos Core.TDict)
        TNat    -> Fix (Core.TBase pos Core.TNat)
      TRef pos x ->
        Fix (Core.TRef pos (dsVar x) 0)
      TGlobal pos Nothing name ->
        Fix (Core.TGlobal pos (dsGlobalName name))
      TGlobal pos (Just _mod) _name ->
        error $ show $ pretty pos <> colon <+> "Qualified names are not yet supported"
      TForall _ bnds body ->
        foldr (\(Binding pos var k) -> Fix . Core.TForall (pos <> Core.getPosition body) (dsVar var) (maybe Core.Star (dsKind pos) k)) body bnds
      TExists _ bnds body ->
        foldr (\(Binding pos var k) -> Fix . Core.TExists pos (dsVar var) (maybe Core.Star (dsKind pos) k)) body bnds
      TArrow pos a b cs ->
        foldl (\acc x -> Core.untele (Fix (Core.TArrow pos)) [acc, x]) a (b:cs)
      TApp _ f a as ->
        Core.untele f (a : as)
      TRecord pos row ->
        let (row', (pvars, rvars)) = dsRow row
            withP k = foldr (\x -> Fix . Core.TExists pos x Core.Presence) k pvars
            withR k = foldr (\x -> Fix . Core.TExists pos x Core.Row) k rvars
        in withR $ withP $ Core.untele (Fix (Core.TRecord pos)) [row']
      TVariant pos row ->
        let (row', (pvars, rvars)) = dsRow row
            withP k = foldr (\x -> Fix . Core.TExists pos x Core.Presence) k pvars
            withR k = foldr (\x -> Fix . Core.TForall pos x Core.Row) k rvars
        in withR $ withP $ Core.untele (Fix (Core.TVariant pos)) [row']
      TArray pos el sz ->
        Core.untele (Fix (Core.TArray pos)) [el, sz]

dsRow :: Row Core.Type -> (Core.Type, ([Core.Var], [Core.Var]))
dsRow t =
  second (\(ps, rs) -> (replicate ps omega, replicate rs rho)) $
  runState (go t) (0, 0)
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
        pure $ Core.untele (Fix (Core.TExtend pos (dsLabel lbl)))
          [ prs', ty, cont']


----------------------------------------------------------------------

data Handlers a = Handlers
  { hNormalise :: Position -> Core.Type -> a
  , hCheck     :: Position -> Core.Type -> Core.Type -> a
  , hImport    :: forall b. Position -> Core.Program b -> Core.Program a
  }

dsStatements :: forall a. Handlers a -> [Statement] -> Core.Program a -> Core.Program a
dsStatements handlers = flip (foldr (dsStatement handlers))

dsStatement :: forall a. Handlers a -> Statement -> Core.Program a -> Core.Program a
dsStatement Handlers{hNormalise, hCheck} stmt cont =
  case stmt of
    Typedef pos name params body ->
      let defn = Core.Definition
            pos
            (dsGlobalName name)
            (dsBindings params)
            (dsType body)
      in Fix (Compose (Core.Now (Core.PLet pos defn cont)))

    Mutualdef pos params clauses ->
      let bindings = dsBindings params
          defs = flip map clauses $ \(MutualClause clausePos name body) ->
            Core.Definition
              clausePos
              (dsGlobalName name)
              bindings
              (dsType body)
      in Fix (Compose (Core.Now (Core.PMutual pos defs cont)))

    Normalise pos ty ->
      let Fix (Compose kont) = cont
      in Fix (Compose (Core.Later (hNormalise pos (dsType ty)) kont))

    Subsume pos sub super ->
      let Fix (Compose kont) = cont
      in Fix (Compose (Core.Later (hCheck pos (dsType sub) (dsType super)) kont))

dsReturn :: Maybe Type -> Core.Program a
dsReturn ty =
  Fix (Compose (Core.Now (maybe Core.PNil (\t -> Core.PReturn (typePos t) (dsType t)) ty)))

dsModule :: forall a. Handlers a -> Module -> Core.Program a
dsModule handlers (Module _name _params _imports statements rettype) =
  dsStatements handlers statements (dsReturn rettype)
