{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module STL.Check where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Foldable (fold)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), cata, para)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Any (..))
import STL.Pretty

import STL.Types
import STL.Eval

----------------------------------------------------------------------
-- Errors

data Err
  = VariableNotFound Position Var Int
  | KindMismatch Position Type Kind Kind
  | ArrowExpected Position Type Kind
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
    VariableNotFound pos x n ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Undefined variable" <+> aVariable (if n > 0 then cpretty x <> "/" <> pretty n else cpretty x) <> "."
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
    ArrowExpected pos t k ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Type application to a non-arrow kinded type:"
        , indent 4 (cpretty t)
        , "Actual kind:"
        , indent 4 (cpretty k)
        ]
    GlobalNotFound pos name ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Undefined global definition" <+> cpretty name <> "."
        ]
    GlobalAlreadyDefined pos name oldpos ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Duplicate global definition" <+> cpretty name <+> ". It has already been defined at:"
        , pretty oldpos
        ]
    IllegalDefinition pos name t reason ->
      nest 4 $ vsep
        [ pretty pos <> ": error:"
        , "Illegal global definition" <+> cpretty name <+> parens (pretty reason) <> colon
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
  { ctxGamma   :: Map Var [Kind]
  , ctxGlobals :: Map GlobalName (Type, Kind)
  }

instance CPretty Ctx where
  cpretty ctx = vsep
    [ "Locals:"
    , indent 2 (ppLocals (ctxGamma ctx)) <> line
    , "Globals:"
    , indent 2 (ppGlobals (ctxGlobals ctx))
    ]
    where
      ppLocals :: Map Var [Kind] -> Doc AnsiStyle
      ppLocals gamma =
        let vars = concatMap (\(var, kinds) -> (,) <$> pure var <*> zip [0..] kinds) $ M.toList gamma
        in vsep $ map ppVar vars

      ppVar :: (Var, (Int, Kind)) -> Doc AnsiStyle
      ppVar (x, (n, k)) = aVariable (cpretty x <> "/" <> pretty n) <+> "->" <+> cpretty k

      ppGlobals :: Map GlobalName (Type, Kind) -> Doc AnsiStyle
      ppGlobals globals = vsep $ map ppDefinition (M.toList globals)

      ppDefinition :: (GlobalName, (Type, Kind)) -> Doc AnsiStyle
      ppDefinition (name, (ty, k)) =
        aKeyword "type" <+> parens (cpretty name <+> colon <+> cpretty k) <+> "=" <+> cpretty ty

----------------------------------------------------------------------
-- Typechecking Monad

type MonadTC m =
  ( MonadError Err m
  , MonadReader Ctx m
  )

lookupCtx :: forall m. (MonadTC m) => Var -> Int -> m (Maybe Kind)
lookupCtx x n = asks $
  listToMaybe . snd . splitAt n . M.findWithDefault [] x . ctxGamma

extendCtx :: forall m a. (MonadTC m) => Var -> Kind -> m a -> m a
extendCtx x k cont = flip local cont $ \ctx ->
  ctx { ctxGamma = M.alter (maybe (Just [k]) (Just . (k :))) x (ctxGamma ctx) }

lookupGlobal :: forall m. (MonadTC m) => GlobalName -> m (Maybe (Type, Kind))
lookupGlobal name = asks $
  M.lookup name . ctxGlobals

withGlobal :: forall m a. (MonadTC m) => GlobalName -> Type -> Kind -> m a -> m a
withGlobal name ty k cont = do
  oldg <- lookupGlobal name
  case oldg of
    Nothing -> local (\ctx -> ctx { ctxGlobals = M.insert name (ty, k) (ctxGlobals ctx) } ) cont
    Just (oldty, _) -> throwError $ GlobalAlreadyDefined (getPosition ty) name (getPosition oldty)

runTC :: ExceptT Err (Reader Ctx) a -> a
runTC k =
  case runIdentity . runTCT $ k of
    Left err -> errorWithoutStackTrace ("\n" ++ show err)
    Right a -> a


runTCT :: (Monad m) => ExceptT Err (ReaderT Ctx m) a -> m (Either (Doc AnsiStyle) a)
runTCT k =
  either (Left . cpretty) Right <$>
    runReaderT (runExceptT k) (Ctx M.empty M.empty)

----------------------------------------------------------------------
-- Kind Inference

inferKind :: forall m. (MonadTC m) => Type -> m Kind
inferKind = para alg
  where
    alg :: TypeF (Type, m Kind) -> m Kind
    alg = \case
      TRef pos x n ->
        lookupCtx x n >>=
        maybe (throwError $ VariableNotFound pos x n) pure
      TGlobal pos name ->
        lookupGlobal name >>=
        maybe (throwError $ GlobalNotFound pos name) (pure . snd)
      TSkolem _ _ _ k ->
        pure k
      TMeta _ _ k ->
        pure k
      TUnit _ ->
        pure Star
      TVoid _ ->
        pure Star
      TArrow _ ->
        pure (Arr Star (Arr Star Star))
      TRecord _ ->
        pure (Arr Row Star)
      TVariant _ ->
        pure (Arr Row Star)
      TPresent _ ->
        pure Presence
      TAbsent _ ->
        pure Presence
      TExtend _ _ ->
        pure (Arr Presence (Arr Star (Arr Row Row)))
      TNil _ ->
        pure Row
      TApp pos (fterm, f) (aterm, a) -> do
        f' <- f
        fterm' <- normalise lookupGlobal fterm
        case f' of
          Arr a'' b'' -> do
            a' <- a
            aterm' <- normalise lookupGlobal aterm
            unless (a' == a'') $
              throwError (KindMismatch (getPosition aterm) (Fix (TApp pos fterm' aterm')) a'' a')
            pure b''
          _other ->
            throwError (ArrowExpected pos (Fix (TApp pos fterm' aterm)) f')
      TLambda _ x k (_, b) -> do
        b' <- extendCtx x k b
        pure (Arr k b')
      TForall _ x k (_, b) -> do
        extendCtx x k b
      TExists _ x k (_, b) -> do
        extendCtx x k b
      TMu pos x (termb, b) -> do
        b' <- extendCtx x Star b
        termb' <- normalise lookupGlobal termb
        let term = Fix (TMu pos x termb')
        unless (b' == Star) $
          throwError (KindMismatch pos term Star b')
        pure Star

inferKindClosed :: Type -> Kind
inferKindClosed ty =
  either (errorWithoutStackTrace . show . cpretty) id $
    runReader (runExceptT (inferKind ty)) (Ctx M.empty M.empty)

----------------------------------------------------------------------
-- Type check program

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
      TMeta _ _ _     -> mempty { containsMetasOrSkolems = Any True }
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

checkProgram :: forall m a. (MonadTC m) => InterleavedProgram (m ()) -> (Maybe Type -> m a) -> m a
checkProgram program cont = cataCompose alg runActions program
  where
    alg :: ProgramF (m a) -> m a
    alg = \case
      PLet pos (Definition _ name params ty) rest -> do
        checkDefinitionTypeWellformedness pos (Just name) ty
        let ty' =
              extendWithParameters pos params $
              handleSelfReference name ty
        kind <- inferKind ty'
        withGlobal name ty' kind $ rest

      PMutual _ _ rest -> do
        -- TODO: Not supported
        rest

      PReturn pos ty -> do
        checkDefinitionTypeWellformedness pos Nothing ty
        kind <- inferKind ty
        unless (kind == Star) $
          throwError $ KindMismatch pos ty Star kind
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
