{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STL.Schema where

import Control.Category ((>>>))

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader

import Data.Functor.Compose
import Data.Functor.Foldable (Fix(..), cata)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void

import qualified STL.Core.Check as Core
import qualified STL.Core.Eval as Core
import qualified STL.Core.Types as Core
import STL.Pretty

----------------------------------------------------------------------
-- Types

newtype Name = Name [Text] deriving (Show, Eq, Ord)
newtype FieldName = FieldName Text deriving (Show, Eq, Ord)
newtype CtorName = CtorName Text deriving (Show, Eq, Ord)
newtype VarName  = VarName Text deriving (Show, Eq, Ord)

data PrimType
  = PUnit
  | PVoid
  | PBool
  | PInt
  | PFloat
  | PString
  deriving (Show, Eq, Ord)

instance CPretty PrimType where
  cpretty = \case
    PUnit   -> aConstructor "Unit"
    PVoid   -> aConstructor "Void"
    PBool   -> aConstructor "Bool"
    PInt    -> aConstructor "Int"
    PFloat  -> aConstructor "Float"
    PString -> aConstructor "String"

data FieldOpt
  = RequiredField
  | OptionalField
  deriving (Show, Eq, Ord)

data SchemaType where
  SParam   :: VarName -> SchemaType
  SNamed   :: Name -> [SchemaType] -> SchemaType
  SPrim    :: PrimType -> SchemaType
  SArrow   :: [SchemaType] -> SchemaType -> SchemaType
  STuple   :: [SchemaType] -> SchemaType
  SArray   :: SchemaType -> SchemaType
  SRecord  :: [(FieldName, FieldOpt, SchemaType)] -> SchemaType
  SVariant :: [(CtorName, Maybe SchemaType)] -> SchemaType
  deriving (Show, Eq, Ord)

instance CPretty SchemaType where
  cpretty = ppSchemaType 0

ppSchemaType :: Int -> SchemaType -> Doc AnsiStyle
ppSchemaType lvl0 = pp lvl0
  where
    pp :: Int -> SchemaType -> Doc AnsiStyle
    pp lvl = \case
      SParam (VarName x) ->
        aVariable (pretty x)
      SNamed (Name xs) as ->
        hsep (aConstructor (hcat $ punctuate dot $ map pretty xs) : map (pp 1) as)
      SPrim prim ->
        cpretty prim
      SArrow fs a ->
        let (open, close) = if lvl > 0 then (lparen, rparen) else (flatAlt space mempty, mempty)
        in ppList open close (flatAlt mempty space <> "→") (map (pp 1) fs ++ [pp 0 a])
      STuple ts ->
        ppList lparen rparen comma (map (pp 0) ts)
      SArray a ->
        brackets (pp 0 a)
      SRecord fs ->
        ppList lbrace rbrace comma (map ppField fs)
      SVariant cs ->
        ppList "⟨" "⟩" (flatAlt mempty space <> pipe) (map ppCtor cs)

    ppOpt :: FieldOpt -> Doc AnsiStyle
    ppOpt = \case
      RequiredField -> mempty
      OptionalField -> "?"

    ppField :: (FieldName, FieldOpt, SchemaType) -> Doc AnsiStyle
    ppField (FieldName lbl, opt, sty) = aLabel (pretty lbl) <> ppOpt opt <+> colon <+> pp 0 sty

    ppCtor :: (CtorName, Maybe SchemaType) -> Doc AnsiStyle
    ppCtor = \case
      (CtorName lbl, Nothing) -> aLabel (pretty lbl)
      (CtorName lbl, Just sty) -> aLabel (pretty lbl) <+> colon <+> pp 0 sty

    ppList :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
    ppList lb rb cm fields =
      group $ align $ enclose (lb <> flatAlt space mempty) (flatAlt line mempty <> rb) $ vcat $
        zipWith (<>) (mempty : repeat (cm <> space)) fields


data SchemaDef = SchemaDef
  { defName   :: Name
  , defParams :: [VarName]
  , defType   :: SchemaType
  } deriving (Show, Eq, Ord)

instance CPretty SchemaDef where
  cpretty (SchemaDef name params body) =
    let (Name nameParts) = name
        pName = hcat $ punctuate "." $ map pretty nameParts
        pParam (VarName x) = aVariable (pretty x)
    in group $ nest 2 $ vsep
       [ aKeyword "type" <+> hsep (pName : map pParam params) <+> "="
       , cpretty body
       ]

----------------------------------------------------------------------
-- Monad

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
    (traverseProgram evalDefinition evalReturn program)
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


traverseProgram :: (Monad m) => (Core.Definition -> m ()) -> (Core.Type -> m ()) -> Core.Program Void -> m ()
traverseProgram f g = cata $ getTerm >>> \case
  Core.PLet _pos def rest -> f def >> rest
  Core.PMutual _pos defs rest -> traverse f defs >> rest
  Core.PReturn _pos ty -> g ty
  Core.PNil -> pure ()
  where
    getTerm :: Compose (Core.ColistF Void) f a -> f a
    getTerm = getCompose >>> \case
      Core.Now a -> a
      Core.Later x _ -> absurd x


evalDefinition :: (MonadEval m) => Core.Definition -> m ()
evalDefinition (Core.Definition pos name params body) = do
  kind <- asks (M.findWithDefault notFoundInternalError name . evGamma)
  ty <- Core.normalise lookupGlobal (Core.etaExpand kind (Core.extendWithParameters pos params body))
  case runExtract (extractDefinition name kind ty) of
    Right def -> emit def
    Left{} -> addGlobal name (Core.normaliseClosed $ Core.extendWithParameters pos params $ Core.handleSelfReference name body)
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
  , exDefName   :: Name
  , exDefParams :: [VarName]
  } deriving (Show, Eq)

type MonadExtract m =
  ( MonadReader ExtractEnv m
  , MonadError () m
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

----------------------------------------------------------------------

runExtract :: ExceptT e (Reader ExtractEnv) a -> Either e a
runExtract = runExceptT >>> flip runReader (ExtractEnv mempty (Name []) [])

extractDefinition :: (MonadExtract m) => Core.GlobalName -> Core.Kind -> Core.Type -> m SchemaDef
extractDefinition (Core.GlobalName n) kind0 type0 = do
  (params, body) <- withDefName (Name [n]) (extractByKind type0 kind0)
  pure (SchemaDef (Name [n]) params body)

extractByKind :: (MonadExtract m) => Core.Type -> Core.Kind -> m ([VarName], SchemaType)
extractByKind ty = \case
  Core.Arr Core.Star _ k -> do
    (var@(Core.Var x), body) <- extractLambda ty
    (xs, st) <- extendCtx var RoleParam $ withDefParam (VarName x) $ extractByKind body k
    pure (VarName x : xs, st)
  Core.Star -> do
    st <- extractRecursion ty
    pure ([], st)
  _other -> do
    throwError ()

extractLambda :: (MonadExtract m) => Core.Type -> m (Core.Var, Core.Type)
extractLambda = \case
  Fix (Core.TLambda _pos x _k _v body) -> pure (x, body)
  _other -> throwError ()

extractRecursion :: (MonadExtract m) => Core.Type -> m SchemaType
extractRecursion = \case
  Fix (Core.TMu _pos x body) -> extendCtx x RoleSelfReference (extractStar body)
  other -> extractStar other

underQuantifiers :: (MonadExtract m) => (Core.Type -> m a) -> Core.Type -> m a
underQuantifiers f = Core.spine >>> \case
  (Fix (Core.TForall _pos x k ty), args) -> do
    role <- case k of
      Core.Row -> pure RoleUniversalTail
      _other   -> throwError ()
    extendCtx x role $
      f (Core.unspine ty args)
  (Fix (Core.TExists _pos x k ty), args) -> do
    role <- case k of
      Core.Star     -> pure RoleExistentialType
      Core.Row      -> pure RoleExistentialTail
      Core.Presence -> pure RoleExistentialPresence
      Core.Nat      -> pure RoleExistentialSize
      _other        -> throwError ()
    extendCtx x role $
      f (Core.unspine ty args)
  (other, args) ->
    f (Core.unspine other args)

extractStar :: (MonadExtract m) => Core.Type -> m SchemaType
extractStar = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x@(Core.Var name) n), []) ->
    lookupCtx x n >>= \case
      RoleParam -> pure $ SParam (VarName name)
      RoleSelfReference -> SNamed <$> asks exDefName <*> asks (map SParam . exDefParams)
      _other -> throwError ()
  (Fix (Core.TGlobal _pos (Core.GlobalName n)), args) -> SNamed (Name [n]) <$> traverse extractStar args
  (Fix (Core.TBase _pos bt), []) -> SPrim <$> extractBaseType bt
  (Fix (Core.TArrow _pos), [a, b]) -> (\a' (as', b') -> SArrow (a' : as') b') <$> extractStar a <*> extractArrow b
  (Fix (Core.TArray _pos), [a, _n]) -> SArray <$> extractStar a -- FIXME: Don't ignore the size
  (Fix (Core.TPair p), args) -> STuple <$> extractTuple (Core.unspine (Fix (Core.TPair p)) args)
  (Fix (Core.TRecord _pos), [r]) -> SRecord <$> extractRecordRows r
  (Fix (Core.TVariant _pos), [r]) -> SVariant <$> extractVariantCases r
  (_other, _args) -> throwError ()

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
      _other -> throwError ()
  (other, args) -> (: []) <$> extractStar (Core.unspine other args)

extractRecordRows :: (MonadExtract m) => Core.Type -> m [(FieldName, FieldOpt, SchemaType)]
extractRecordRows = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleUniversalTail -> pure []
      RoleExistentialTail -> pure []
      _other -> throwError ()
  (Fix (Core.TNil _pos), []) -> pure []
  (Fix (Core.TExtend _pos (Core.Label lbl)), [presence, ty, row]) -> do
    rest <- extractRecordRows row
    extractPresence presence >>= \case
      Nothing -> pure rest
      Just optionality -> do
        ty' <- extractStar ty
        pure $ (FieldName lbl, optionality, ty') : rest
  _ -> throwError ()

extractVariantCases :: (MonadExtract m) => Core.Type -> m [(CtorName, Maybe SchemaType)]
extractVariantCases = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleUniversalTail -> pure []
      RoleExistentialTail -> pure []
      _other -> throwError ()
  (Fix (Core.TNil _pos), []) -> pure []
  (Fix (Core.TExtend _pos (Core.Label lbl)), [presence, ty, row]) -> do
    rest <- extractVariantCases row
    extractPresence presence >>= \case
      Nothing -> pure rest
      Just{} -> do
        extractStar ty >>= \case
          SPrim PUnit -> pure $ (CtorName lbl, Nothing) : rest
          sty         -> pure $ (CtorName lbl, Just sty) : rest
  _ -> throwError ()

extractPresence :: (MonadExtract m) => Core.Type -> m (Maybe FieldOpt)
extractPresence = underQuantifiers $ Core.spine >>> \case
  (Fix (Core.TRef _pos x n), []) ->
    lookupCtx x n >>= \case
      RoleExistentialPresence -> pure (Just OptionalField)
      _other -> throwError ()
  (Fix (Core.TPresent _pos), []) -> pure (Just RequiredField)
  (Fix (Core.TAbsent  _pos), []) -> pure Nothing
  _ -> throwError ()

extractBaseType :: (MonadExtract m) => Core.BaseType -> m PrimType
extractBaseType = \case
  Core.TUnit   -> pure PUnit
  Core.TVoid   -> pure PVoid
  Core.TBool   -> pure PBool
  Core.TInt    -> pure PInt
  Core.TFloat  -> pure PFloat
  Core.TString -> pure PString
  Core.TDict   -> throwError ()
  Core.TNat    -> throwError ()
