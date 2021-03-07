{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module STL.Schema.Extract
  ( genSchema
  , genSchema'
  ) where

import Control.Category ((>>>))

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader

import Data.Fix (Fix(..))
import Data.Functor.Compose
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Void

import qualified STL.Core.Check as Core
import qualified STL.Core.Eval as Core
import qualified STL.Core.Types as Core
import STL.Pretty
import STL.Schema.Types


data EvalEnv = EvalEnv
  { evGamma :: Map Core.GlobalName Core.Kind
  }

data EvalState = EvalState
  { stScope :: Map Core.GlobalName Core.Type
  }

data Output = Output { runOutput :: [SchemaDef] -> [SchemaDef] }

instance Semigroup Output where
  Output f <> Output g = Output (f . g)

instance Monoid Output where
  mempty = Output id

type MonadEval m =
  ( MonadState EvalState m
  , MonadReader EvalEnv m
  , MonadWriter Output m
  , MonadError (Doc AnsiStyle) m
  )

lookupGlobal :: (MonadState EvalState m) => Core.GlobalName -> m (Maybe Core.Type)
lookupGlobal name = gets (M.lookup name . stScope)

addGlobal :: (MonadState EvalState m) => Core.GlobalName -> Core.Type -> m ()
addGlobal name ty = do
  modify (\st -> st {stScope = M.insert name ty (stScope st) })

emit :: (MonadWriter Output m) => SchemaDef -> m ()
emit d = tell (Output (d:))

----------------------------------------------------------------------

genSchema :: Core.Program Void -> Either (Doc AnsiStyle) (Doc AnsiStyle)
genSchema program =
  case genSchema' program of
    Left err -> Left err
    Right as -> Right $ vsep (map cpretty as)


genSchema' :: (MonadError (Doc AnsiStyle) m) => Core.Program Void -> m [SchemaDef]
genSchema' program = do
  gamma <- either throwError pure =<< Core.runTCT
    (Core.checkProgram
      (Core.purifyProgram program)
      (\_ -> asks (fmap snd . Core.ctxGlobals)))
  (_, _, output) <- runRWST
    (traverseProgram evalDefinitionGroup evalReturn program)
    (initEnv gamma)
    initState
  pure (runOutput output [])
  where
    initEnv gamma = EvalEnv
      { evGamma = gamma
      }
    initState = EvalState
      { stScope = mempty
      }


traverseProgram :: (Monad m) => ([Core.Definition] -> m ()) -> (Core.Type -> m ()) -> Core.Program Void -> m ()
traverseProgram f g = cata $ getTerm >>> \case
  Core.PLet _pos def rest -> f [def] >> rest
  Core.PMutual _pos defs rest -> f defs >> rest
  Core.PReturn _pos ty -> g ty
  Core.PNil -> pure ()
  where
    getTerm :: Compose (Core.ColistF Void) f a -> f a
    getTerm = getCompose >>> \case
      Core.Now a -> a
      Core.Later x _ -> absurd x


evalDefinitionGroup :: (MonadEval m) => [Core.Definition] -> m ()
evalDefinitionGroup defs =
  let names = map Core._defName defs
  in mapM_ (evalDefinition names) defs

evalDefinition :: (MonadEval m) => [Core.GlobalName] -> Core.Definition -> m ()
evalDefinition groupNames (Core.Definition pos name params body) = do
  kind <- asks (M.findWithDefault notFoundInternalError name . evGamma)
  ty <- Core.normalise lookupGlobal (Core.etaExpand kind (Core.extendWithParameters pos params body))
  case runExtract (extractDefinition groupNames name kind ty) of
    Right def -> emit def
    Left _reason ->
      addGlobal name $
        Core.normaliseClosed $
        Core.extendWithParameters pos params $
        Core.handleSelfReference name body
  where
    notFoundInternalError = error $ "internal error: global not found: " ++ show name


evalReturn :: (MonadEval m) => Core.Type -> m ()
evalReturn ty = do
  ty' <- Core.normalise lookupGlobal ty
  case runExtract (extractStar ty') of
    Left{} -> pure ()
    Right sty -> emit (SchemaDef (Name ["API"]) [] sty)

----------------------------------------------------------------------
-- Extracting

data VarRole
  = RoleParam
  | RoleUniversalTail
  | RoleExistentialType
  | RoleExistentialTail
  | RoleExistentialPresence
  | RoleExistentialSize
  | RoleSelfReference
    deriving (Show, Eq)

data ExtractEnv = ExtractEnv
  { exGamma     :: M.Map Core.Var [VarRole]
  , exDefGroup  :: [Name]
  , exDefName   :: Name
  , exDefParams :: [VarName]
  } deriving (Show, Eq)

type MonadExtract m =
  ( MonadReader ExtractEnv m
  , MonadError (Doc AnsiStyle) m
  )

----------------------------------------------------------------------

lookupCtx :: (MonadExtract m) => Core.Var -> Int -> m VarRole
lookupCtx x n =
  asks ((!! n) . M.findWithDefault [] x . exGamma)

extendCtx :: (MonadExtract m) => Core.Var -> VarRole -> m a -> m a
extendCtx x r =
  local (\env -> env {exGamma = M.alter (maybe (Just [r]) (Just . (r :))) x (exGamma env)} )

withDefName :: (MonadExtract m) => Name -> m a -> m a
withDefName name =
  local (\env -> env {exDefName = name})

withDefParam :: (MonadExtract m) => VarName -> m a -> m a
withDefParam param =
  local (\env -> env {exDefParams = param : exDefParams env})

withDefGroup :: (MonadExtract m) => [Name] -> m a -> m a
withDefGroup names =
  local (\env -> env {exDefGroup = names})

----------------------------------------------------------------------

runExtract :: ExceptT e (Reader ExtractEnv) a -> Either e a
runExtract = runExceptT >>> flip runReader (ExtractEnv mempty [] (Name []) [])

extractName :: Core.GlobalName -> Name
extractName (Core.GlobalName n) = Name [n]

extractDefinition :: (MonadExtract m) => [Core.GlobalName] -> Core.GlobalName -> Core.Kind -> Core.Type -> m SchemaDef
extractDefinition groupNames name kind0 type0 = do
  let name' = extractName name
  (params, body) <-
    withDefName name' $
      withDefGroup (map extractName groupNames) $
        extractByKind type0 kind0
  pure (SchemaDef name' params body)

extractByKind :: (MonadExtract m) => Core.Type -> Core.Kind -> m ([VarName], SchemaType)
extractByKind ty = \case
  Core.Arr Core.Star _ k -> do
    (var@(Core.Var x), body) <- extractLambda ty
    (xs, st) <- extendCtx var RoleParam $ withDefParam (VarName x) $ extractByKind body k
    pure (VarName x : xs, st)
  Core.Star -> do
    st <- extractRecursion ty
    pure ([], st)
  other -> do
    throwError $ "Unexpected kind:" <+> cpretty other

extractLambda :: (MonadExtract m) => Core.Type -> m (Core.Var, Core.Type)
extractLambda = \case
  Fix (Core.TLambda _pos x _k _v body) -> pure (x, body)
  other -> throwError $ "Unexpected type instead of lambda:" <+> cpretty other

extractRecursion :: (MonadExtract m) => Core.Type -> m SchemaType
extractRecursion = \case
  Fix (Core.TMu _pos x body) -> extendCtx x RoleSelfReference (extractStar body)
  other -> extractStar other

underQuantifiers :: (MonadExtract m) => (Core.Type -> m a) -> Core.Type -> m a
underQuantifiers f = Core.spine >>> \case
  (Fix (Core.TForall _pos x k ty), args) -> do
    role <- case k of
      Core.Row -> pure RoleUniversalTail
      other    -> throwError $ "Universal quantifier of unexpected kind:" <+> cpretty other
    extendCtx x role $
      underQuantifiers f (Core.unspine ty args)
  (Fix (Core.TExists _pos x k ty), args) -> do
    role <- case k of
      Core.Star     -> pure RoleExistentialType
      Core.Row      -> pure RoleExistentialTail
      Core.Presence -> pure RoleExistentialPresence
      Core.Nat      -> pure RoleExistentialSize
      other         -> throwError $ "Existential quantifier of unexpected kind:" <+> cpretty other
    extendCtx x role $
      underQuantifiers f (Core.unspine ty args)
  (other, args) ->
    f (Core.unspine other args)

extractStar :: (MonadExtract m) => Core.Type -> m SchemaType
extractStar = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x@(Core.Var name) n), []) ->
    lookupCtx x n >>= \case
      RoleParam -> pure $ SParam (VarName name)
      RoleSelfReference -> SNamed <$> asks exDefName <*> asks (map SParam . exDefParams)
      _other -> throwError "Unexpected variable reference"
  (Fix (Core.TGlobal _pos name), args) -> do
    let name' = extractName name
    recursive <- asks (elem name' . exDefGroup)
    case (recursive, args) of
      (False, _) -> SNamed name' <$> traverse extractStar args
      (True, []) -> SNamed name' <$> asks (map SParam . exDefParams)
      (True, _)  -> error "internal error: explicit arguments to self-reference"
  (Fix (Core.TBase _pos bt), []) -> SPrim <$> extractBaseType bt
  (Fix (Core.TArrow _pos), [a, b]) -> (\a' (as', b') -> SArrow (a' : as') b') <$> extractStar a <*> extractArrow b
  (Fix (Core.TArray _pos), [a, _n]) -> SArray <$> extractStar a -- FIXME: Don't ignore the size
  (Fix (Core.TPair p), args) -> STuple <$> extractTuple (Core.unspine (Fix (Core.TPair p)) args)
  (Fix (Core.TRecord _pos), [r]) -> SRecord <$> extractRecordRows r
  (Fix (Core.TVariant _pos), [r]) -> SVariant <$> extractVariantCases r
  other -> throwError $ "Unexpected type:" <+> cpretty (uncurry Core.unspine other)

extractArrow :: (MonadExtract m) => Core.Type -> m ([SchemaType], SchemaType)
extractArrow = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TArrow _pos), [a, b]) -> do
    a' <- extractStar a
    (as', b') <- extractArrow b
    pure (a' : as', b')
  (other, args) -> do
    b' <- extractStar (Core.unspine other args)
    pure ([], b')

extractTuple :: (MonadExtract m) => Core.Type -> m [SchemaType]
extractTuple = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TPair _), [a, b]) -> (:) <$> extractStar a <*> extractTuple b
  (Fix (Core.TRef _pos x@(Core.Var name) n), []) ->
    lookupCtx x n >>= \case
      RoleParam -> pure [SParam (VarName name)]
      RoleExistentialType -> pure []
      _other -> throwError "Unexpected variable reference"
  (other, args) -> (: []) <$> extractStar (Core.unspine other args)

extractRecordRows :: (MonadExtract m) => Core.Type -> m [(FieldName, FieldOpt, SchemaType)]
extractRecordRows = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleUniversalTail -> pure []
      RoleExistentialTail -> pure []
      _other -> throwError "Unexpected variable reference"
  (Fix (Core.TNil _pos), []) -> pure []
  (Fix (Core.TExtend _pos (Core.Label lbl)), [presence, ty, row]) -> do
    rest <- extractRecordRows row
    extractPresence presence >>= \case
      Nothing -> pure rest
      Just optionality -> do
        ty' <- extractStar ty
        pure $ (FieldName lbl, optionality, ty') : rest
  other -> throwError $ "Unexpected row-type:" <+> cpretty (uncurry Core.unspine other)

extractVariantCases :: (MonadExtract m) => Core.Type -> m [(CtorName, Maybe SchemaType)]
extractVariantCases = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleUniversalTail -> pure []
      RoleExistentialTail -> pure []
      _other -> throwError "Unexpected variable reference"
  (Fix (Core.TNil _pos), []) -> pure []
  (Fix (Core.TExtend _pos (Core.Label lbl)), [presence, ty, row]) -> do
    rest <- extractVariantCases row
    extractPresence presence >>= \case
      Nothing -> pure rest
      Just{} -> do
        extractStar ty >>= \case
          SPrim PUnit -> pure $ (CtorName lbl, Nothing) : rest
          sty         -> pure $ (CtorName lbl, Just sty) : rest
  other -> throwError $ "Unexpected row-type:" <+> cpretty (uncurry Core.unspine other)

extractPresence :: (MonadExtract m) => Core.Type -> m (Maybe FieldOpt)
extractPresence = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleExistentialPresence -> pure (Just OptionalField)
      _other -> throwError "Unexpected variable reference"
  (Fix (Core.TPresent _pos), []) -> pure (Just RequiredField)
  (Fix (Core.TAbsent  _pos), []) -> pure Nothing
  other -> throwError $ "Unexpected presence-type:" <+> cpretty (uncurry Core.unspine other)

extractBaseType :: (MonadExtract m) => Core.BaseType -> m PrimType
extractBaseType = \case
  Core.TUnit   -> pure PUnit
  Core.TVoid   -> pure PVoid
  Core.TBool   -> pure PBool
  Core.TInt    -> pure PInt
  Core.TFloat  -> pure PFloat
  Core.TString -> pure PString
  Core.TDict   -> throwError "Dicts are not supported"
  Core.TNat    -> throwError "Nats are not supported"
