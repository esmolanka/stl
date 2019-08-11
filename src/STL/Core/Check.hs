{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Core.Check where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import Data.Foldable (fold)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), cata, para)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Any (..))
import STL.Pretty

import STL.Core.Types
import STL.Core.Eval

----------------------------------------------------------------------
-- Kind inference types

data KindExpectation
  = NoExpectation
  | ExpectArrow KindExpectation KindExpectation
  | ExpectExactly Kind

instance CPretty KindExpectation where
  cpretty expectation = evalState (ppKindExpectation False expectation) 1
    where
      ppKindExpectation :: Bool -> KindExpectation -> State Int (Doc AnsiStyle)
      ppKindExpectation nested = \case
        NoExpectation -> do
          n <- get
          modify succ
          pure (aVariable ("κ" <> ppSubscript n))
        ExpectArrow a b -> do
          a' <- ppKindExpectation False a
          b' <- ppKindExpectation False b
          pure (a' <+> aKind "->" <+> b')
        ExpectExactly k -> pure (ppKind nested k)

data Polarity
  = Not Polarity
  | Positive
    deriving (Eq, Ord)

data Flavour
  = Universal
  | Existential
  | Parameter
  | Recursion

data VarInfo = VarInfo
  { varKind     :: Kind
  , varFlavour  :: Flavour
  , varPolarity :: Maybe Polarity
  }

----------------------------------------------------------------------
-- Errors

data Err
  = VariableNotFound Position Var Int KindExpectation
  | IllegalNegativePolarity Position Var Int Flavour
  | KindMismatch Position Type KindExpectation Kind
    -- Globals
  | GlobalNotFound Position GlobalName
  | GlobalAlreadyDefined Position GlobalName Position
  | IllegalDefinition Position GlobalName Type IllegalDefinitionReason

data IllegalDefinitionReason
  = DefinitionContainsMetasOrSkolems
  | DefinitionContainsExplicitRecursion
  | DefinitionContainsExplicitParametrisation

instance CPretty Err where
  cpretty = \case
    VariableNotFound pos x n expectation ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , case expectation of
            NoExpectation ->
              "Undefined variable" <+> squotes (ppVar x n) <> "."
            _other ->
              "Undefined variable" <+> squotes (ppVar x n) <+> "of kind" <+> cpretty expectation <> "."
        ]
    IllegalNegativePolarity pos x n flavour ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , case flavour of
            Parameter ->
              "Parameter" <+> squotes (ppVar x n) <+> "must not be used in a function argument."
            Recursion ->
              "Recursive reference" <+> squotes (ppVar x n) <+> "must not occur in a function argument."
            _other ->
              "Strictly positive variable" <+> squotes (ppVar x n) <+> "must not be used in a function argument."
        ]
    KindMismatch pos t k k' ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Kind mismatch while checking type:"
        , indent 4 (cpretty t)
        , "Expected kind:"
        , indent 4 (cpretty k)
        , "Actual kind:"
        , indent 4 (cpretty k')
        ]
    GlobalNotFound pos name ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Undefined type" <+> squotes (cpretty name) <> "."
        ]
    GlobalAlreadyDefined pos name oldpos ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Duplicate type definition" <+> squotes (cpretty name) <> ". It has been already defined at:"
        , pretty oldpos
        ]
    IllegalDefinition pos name t reason ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Illegal global definition" <+> squotes (cpretty name) <+> parens (pretty reason) <> colon
        , indent 4 (cpretty t)
        ]

instance Pretty IllegalDefinitionReason where
  pretty = \case
    DefinitionContainsMetasOrSkolems ->
      "contains meta variables or skolems"
    DefinitionContainsExplicitRecursion ->
      "contains explicit recursion"
    DefinitionContainsExplicitParametrisation ->
      "contains non-top-level parametrisation"

----------------------------------------------------------------------
-- Context

data Ctx = Ctx
  { ctxGamma    :: Map Var [VarInfo]
  , ctxGlobals  :: Map GlobalName (Type, Kind)
  , ctxKindAnn  :: KindExpectation
  , ctxPolarity :: Polarity
  }

instance CPretty Ctx where
  cpretty ctx = vsep
    [ "Locals:"
    , indent 2 (ppLocals (ctxPolarity ctx) (ctxGamma ctx)) <> line
    , "Globals:"
    , indent 2 (ppGlobals (ctxGlobals ctx))
    ]
    where
      ppLocals :: Polarity -> Map Var [VarInfo] -> Doc AnsiStyle
      ppLocals pol gamma =
        let vars = concatMap (\(var, kinds) -> (,) <$> pure var <*> zip [0..] kinds) $ M.toList gamma
        in vsep $ map (ppVarInfo pol) vars

      ppVarInfo :: Polarity -> (Var, (Int, VarInfo)) -> Doc AnsiStyle
      ppVarInfo pol (x, (n, (VarInfo kind _ vpol))) =
        ppVar x n <+> ppPolarity pol vpol <+> "->" <+> cpretty kind

      ppPolarity :: Polarity -> Maybe Polarity -> Doc AnsiStyle
      ppPolarity pol = \case
        Nothing   -> "?"
        Just pol' | pol == pol' -> "+"
                  | otherwise   -> "~"

      ppGlobals :: Map GlobalName (Type, Kind) -> Doc AnsiStyle
      ppGlobals globals = vsep $ map ppDefinition (M.toList globals)

      ppDefinition :: (GlobalName, (Type, Kind)) -> Doc AnsiStyle
      ppDefinition (name, (ty, k)) =
        aKeyword "type" <+> parens (cpretty name <+> colon <+> cpretty k) <+> "=" <+> cpretty ty

----------------------------------------------------------------------
-- Kind checking monad

type MonadTC m =
  ( MonadError Err m
  , MonadReader Ctx m
  )

-- Variables

lookupCtx :: forall m. (MonadTC m) => Var -> Int -> m (Maybe VarInfo)
lookupCtx x n = asks $ \ctx ->
  listToMaybe . snd . splitAt n .
    M.findWithDefault [] x . ctxGamma $ ctx

extendCtx :: forall m a. (MonadTC m) => Var -> Flavour -> Kind -> m a -> m a
extendCtx x flavour kind cont = flip local cont $ \ctx ->
  let info = VarInfo
        kind
        flavour
        (case flavour of
           Parameter -> Just (ctxPolarity ctx)
           Recursion -> Just (ctxPolarity ctx)
           _other    -> Nothing)

  in ctx { ctxGamma = M.alter (maybe (Just [info]) (Just . (info :))) x (ctxGamma ctx) }

-- Globals

lookupGlobal :: forall m. (MonadTC m) => GlobalName -> m (Maybe (Type, Kind))
lookupGlobal name = asks $
  M.lookup name . ctxGlobals

withGlobal :: forall m a. (MonadTC m) => Position -> GlobalName -> Type -> Kind -> m a -> m a
withGlobal pos name ty k cont = do
  oldg <- lookupGlobal name
  case oldg of
    Nothing -> local (\ctx -> ctx { ctxGlobals = M.insert name (ty, k) (ctxGlobals ctx) } ) cont
    Just (oldty, _) -> throwError $ GlobalAlreadyDefined pos name (getPosition oldty)

-- Bidirectional kind checking

expectAny :: forall m a. (MonadTC m) => m a -> m a
expectAny = local
  (\ctx -> ctx { ctxKindAnn = NoExpectation })

expectArrowPush :: forall m a. (MonadTC m) => Kind -> m a -> m a
expectArrowPush arg = local
  (\ctx -> ctx { ctxKindAnn = ExpectArrow (ExpectExactly arg) (ctxKindAnn ctx) })

expectArrowPushAny :: forall m a. (MonadTC m) => m a -> m a
expectArrowPushAny = local
  (\ctx -> ctx { ctxKindAnn = ExpectArrow NoExpectation (ctxKindAnn ctx) })

expectArrowPop :: forall m a. (MonadTC m) => m a -> m a
expectArrowPop = local
  (\ctx -> ctx { ctxKindAnn = case ctxKindAnn ctx of
                                NoExpectation -> NoExpectation
                                ExpectArrow _ ex -> ex
                                ExpectExactly (Arr _ b) -> ExpectExactly b
                                ExpectExactly _ -> NoExpectation })

expectExactly :: forall m a. (MonadTC m) => Kind -> m a -> m a
expectExactly k = local
  (\ctx -> ctx { ctxKindAnn = ExpectExactly k })

is :: forall m. (MonadTC m) => Type -> Kind -> m Kind
is typ kind = do
  expectation <- asks ctxKindAnn
  unless (matchExpectation kind expectation) $
    throwError $ KindMismatch (getPosition typ) typ expectation kind
  pure kind
  where
    matchExpectation :: Kind -> KindExpectation -> Bool
    matchExpectation k = \case
      NoExpectation -> True
      ExpectExactly k' -> k == k'
      ExpectArrow argExp restExpectation ->
        case k of
          Arr arg restKind ->
            matchExpectation arg argExp &&
              matchExpectation restKind restExpectation
          _other -> False

getExpectation :: forall m. (MonadTC m) => m KindExpectation
getExpectation = asks ctxKindAnn

-- Strict positivity checking

flippedPolarity :: forall m a. (MonadTC m) => m a -> m a
flippedPolarity = local (\ctx -> ctx { ctxPolarity = Not (ctxPolarity ctx) })

checkPolarity :: forall m. (MonadTC m) => Polarity -> m Bool
checkPolarity pol = asks ((pol ==) . ctxPolarity)

-- Backtracking

try :: forall m a. (MonadTC m) => m a -> m (Maybe a)
try k = fmap Just k `catchError` (\_ -> pure Nothing)

-- Run TC monad

runTC :: ExceptT Err (Reader Ctx) a -> a
runTC k =
  case runIdentity . runTCT $ k of
    Left err -> errorWithoutStackTrace ("\n" ++ show err)
    Right a -> a

runTCT :: (Monad m) => ExceptT Err (ReaderT Ctx m) a -> m (Either (Doc AnsiStyle) a)
runTCT k =
  either (Left . cpretty) Right <$>
    runReaderT (runExceptT k) (Ctx M.empty M.empty NoExpectation Positive)

----------------------------------------------------------------------
-- Kind check type terms

inferKind :: forall m. (MonadTC m) => Type -> m Kind
inferKind = para alg
  where
    alg :: TypeF (Type, m Kind) -> m Kind
    alg layer = let this = Fix (fmap fst layer) in case (fmap snd layer) of
      TRef pos x n ->
        lookupCtx x n >>= \case
          Nothing -> do
            expectation <- getExpectation
            throwError $ VariableNotFound pos x n expectation
          Just (VarInfo kind flavour (Just pol)) -> do
            ok <- checkPolarity pol
            unless ok $
              throwError $ IllegalNegativePolarity pos x n flavour
            this `is` kind
          Just (VarInfo kind _ _) -> this `is` kind
      TGlobal pos name ->
        lookupGlobal name >>=
        maybe (throwError $ GlobalNotFound pos name) ((this `is`) . snd)
      TSkolem _ _ _ k ->
        this `is` k
      TMeta _ _ _ k ->
        this `is` k
      TBase _ base ->
        this `is` baseKind base
      TArrow _ ->
        this `is` Arr Star (Arr Star Star)
      TRecord _ ->
        this `is` Arr Row Star
      TVariant _ ->
        this `is` Arr Row Star
      TArray _ ->
        this `is` Arr Star (Arr Nat Star)
      TPresent _ ->
        this `is` Presence
      TAbsent _ ->
        this `is` Presence
      TExtend _ _ ->
        this `is` Arr Presence (Arr Star (Arr Row Row))
      TNil _ ->
        this `is` Row
      TApp pos f a -> do
        let checkArg =
              case this of
                Fix (TApp _ (Fix (TArrow _)) _) -> flippedPolarity a
                _other                          -> a
        ma' <- try $ expectAny $ checkArg
        f' <- maybe expectArrowPushAny expectArrowPush ma' $ f
        case f' of
          Arr a' b' -> do
            case ma' of
              Just _ -> pure ()
              Nothing -> void (expectExactly a' checkArg)
            this `is` b'
          _other -> error $ show $ pretty pos <> colon <+> "internal error: unexpected non-arrow kind"
      TLambda _ x k b -> do
        b' <- expectArrowPop (extendCtx x Parameter k b)
        this `is` Arr k b'
      TForall _ x k b ->
        extendCtx x Universal k b
      TExists _ x k b ->
        extendCtx x Existential k b
      TMu _ x b -> do
        _ <- expectExactly Star (extendCtx x Recursion Star b)
        this `is` Star

baseKind :: BaseType -> Kind
baseKind = \case
  TUnit   -> Star
  TVoid   -> Star
  TBool   -> Star
  TInt    -> Star
  TFloat  -> Star
  TString -> Star
  TList   -> Arr Star Star
  TDict   -> Arr Star Star
  TNat    -> Arr Nat Star


inferKindClosed :: Type -> Kind
inferKindClosed ty =
  either (errorWithoutStackTrace . show . cpretty) id $
    runReader (runExceptT (inferKind ty)) (Ctx M.empty M.empty NoExpectation Positive)

----------------------------------------------------------------------
-- Kind check programs

data ContainsNodes m = ContainsNodes
  { containsLambdas :: m
  , containsMus :: m
  , containsMetasOrSkolems :: m
  }

instance Monoid m => Monoid (ContainsNodes m) where
  mempty = ContainsNodes mempty mempty mempty

instance Semigroup m => Semigroup (ContainsNodes m) where
  ContainsNodes x y z <> ContainsNodes x' y' z' =
    ContainsNodes (x <> x') (y <> y') (z <> z')

containsNodes :: Type -> ContainsNodes Bool
containsNodes = getAnys . cata alg
  where
    getAnys :: ContainsNodes Any -> ContainsNodes Bool
    getAnys (ContainsNodes x y z) =
      ContainsNodes (getAny x) (getAny y) (getAny z)

    alg :: TypeF (ContainsNodes Any) -> ContainsNodes Any
    alg = \case
      TLambda _ _ _ _ -> mempty { containsLambdas = Any True }
      TMu _ _ _       -> mempty { containsMus = Any True }
      TMeta _ _ _ _   -> mempty { containsMetasOrSkolems = Any True }
      TSkolem _ _ _ _ -> mempty { containsMetasOrSkolems = Any True }
      other -> fold other

handleSelfReference :: GlobalName -> Type -> Type
handleSelfReference name@(GlobalName dname) ty =
  if containsRecursion ty
  then Fix $ TMu (getPosition ty) (Var dname) $
         substGlobal name (\p -> Fix (TRef p (Var dname) 0)) $
           shift 1 (Var dname) ty
  else ty
  where
    containsRecursion :: Type -> Bool
    containsRecursion = (getAny .) . cata $ \case
      TGlobal _ name' -> Any (name == name')
      other -> fold other

extendWithParameters :: Position -> [(Var, Kind)] -> Type -> Type
extendWithParameters pos params ty =
  foldr (\(x, k) -> Fix . TLambda pos x k) ty params

checkDefinitionTypeWellformedness :: forall m. (MonadTC m) => Position -> Maybe GlobalName -> Type -> m ()
checkDefinitionTypeWellformedness pos name' ty = do
  let name = fromMaybe (GlobalName "return") name'
      nodes = containsNodes ty
      illegalError reason = IllegalDefinition pos name ty reason
  when (containsLambdas nodes) $
    throwError $ illegalError DefinitionContainsExplicitParametrisation
  when (containsMus nodes) $
    throwError $ illegalError DefinitionContainsExplicitRecursion
  when (containsMetasOrSkolems nodes) $
    throwError $ illegalError DefinitionContainsMetasOrSkolems

checkProgram :: forall m a. (MonadTC m) => Program (m ()) -> (Maybe Type -> m a) -> m a
checkProgram program cont = cataCompose alg runActions program
  where
    alg :: ProgramF (m a) -> m a
    alg = \case
      PLet pos (Definition _ name params ty) rest -> do
        checkDefinitionTypeWellformedness pos (Just name) ty
        let ty' =
              extendWithParameters pos params $
              handleSelfReference name ty
        kind <- expectAny (inferKind ty')
        withGlobal pos name ty' kind rest

      PMutual _ _ rest -> do
        -- TODO: Not supported
        rest

      PReturn pos ty -> do
        checkDefinitionTypeWellformedness pos Nothing ty
        _ <- expectExactly Star (inferKind ty)
        cont (Just ty)

      PNil ->
        cont Nothing

    runActions :: ColistF (m ()) (m a) -> m a
    runActions = \case
      Now a -> a
      Later action k -> action >> runActions k

----------------------------------------------------------------------

cataCompose :: (Functor f, Functor g) => (g a -> b) -> (f b -> a) -> Fix (Compose f g) -> a
cataCompose g f = cata (f . fmap g . getCompose)
