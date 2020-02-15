{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main, runModule) where

import qualified System.IO as IO
import System.Console.Haskeline
import System.Exit

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Functor.Foldable (Fix(..))

import qualified Options.Applicative as Opt

import STL
import STL.Core.Check
import STL.Core.Subsumption
import STL.Elab (dsStatement, dsReturn, dsGlobalName, dsModule, Handlers(..))
import STL.Pretty hiding (list)
import STL.Syntax (parseStatement, parseModule, Statement(..))

----------------------------------------------------------------------
-- Utils

output :: (MonadIO m) => Doc AnsiStyle -> m ()
output doc = liftIO $ putDocLn doc

check :: (MonadTC m, MonadIO m) => Position -> Type -> Type -> m ()
check pos sub sup = do
  k <- inferKind sub
  void $ expectExactly k $ inferKind sup
  sub' <- normalise lookupGlobal sub
  sup' <- normalise lookupGlobal sup
  let (res, state) = runSubsumption mempty (sub' `subsumedBy` sup')
  output $ nest 2 $ vsep
    [ pretty pos <> colon <+> either (const "error: subsumption:") (const "subsumption:") res
    , cpretty sub'
    , indent 2 "<:"
    , cpretty sup'
    , mempty
    , either cpretty (\_ -> "OK") $ res
    , mempty
    , "State:" <+> align (cpretty state)
    , mempty
    ]

eval :: (MonadTC m, MonadIO m) => Position -> Type -> m ()
eval pos ty = do
  k   <- inferKind ty
  ty' <- normalise lookupGlobal ty
  void $ expectExactly k $ inferKind ty'
  output $ nest 2 $ vsep
    [ pretty pos <> colon <+> "normalisation:"
    , cpretty ty'
    , ":" <+> cpretty k
    , mempty
    ]

mainHandlers :: (MonadTC m, MonadIO m) => Handlers (m ())
mainHandlers = Handlers eval check (\_ k -> purifyProgram k)

silentHandlers :: (MonadTC m, MonadIO m) => Handlers (m ())
silentHandlers = Handlers (\_ _ -> pure ()) (\_ _ _ -> pure ()) (\_ k -> purifyProgram k)

runModule :: (MonadIO m) => Handlers (ExceptT Err (ReaderT Ctx m) ()) -> FilePath -> m (Either (Doc AnsiStyle) ())
runModule handlers fn = do
  str <- liftIO $ BL.readFile fn
  case parseModule fn str of
    Left err ->
      pure (Left (pretty err))
    Right modul ->
      runTCT $ checkProgram (dsModule handlers modul) $ \case
        Just ty -> hNormalise handlers (getPosition ty) ty
        Nothing -> pure ()

runModuleRetType :: (MonadIO m) => FilePath -> m (Either (Doc AnsiStyle) Type)
runModuleRetType fn = do
  str <- liftIO $ BL.readFile fn
  case parseModule fn str of
    Left err ->
      pure (Left (pretty err))
    Right modul ->
      fmap (>>= maybe (throwError $ pretty fn <> ": does not provide a type.") pure) $
      runTCT $ checkProgram (dsModule silentHandlers modul) $ \case
        Just ty -> do
          void $ inferKind ty
          ty' <- normalise lookupGlobal ty
          pure (Just ty')
        Nothing ->
          pure Nothing

----------------------------------------------------------------------

data Config = Config
  { cfgFileName :: Maybe FilePath
  , cfgCheckAgainst :: Maybe FilePath
  }

config :: Opt.Parser Config
config =
  Config
    <$> (Opt.optional $ Opt.strArgument $
           Opt.metavar "SRC" <>
           Opt.help "Source .stl file to process")
    <*> (Opt.optional $ Opt.strOption $
          Opt.long "against" <>
          Opt.metavar "SRC" <>
          Opt.help "Supertype .stl file to check against")

main :: IO ()
main = do
  IO.hSetEncoding IO.stdin  IO.utf8
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

  cfg <- Opt.execParser $
    Opt.info
      (Opt.helper <*> config)
      (Opt.fullDesc <>
       Opt.progDesc "Structural Types Langauge")

  case (cfgFileName cfg, cfgCheckAgainst cfg) of
    (Just fn, Nothing) ->
      runModule mainHandlers fn >>=
      either (\err -> putDocLn err >> exitFailure) pure

    (Just sub, Just sup) -> do
      subTy <- runModuleRetType sub >>= either (\err -> putDocLn err >> exitFailure) pure
      supTy <- runModuleRetType sup >>= either (\err -> putDocLn err >> exitFailure) pure
      let (res, _) = runSubsumption mempty (subTy `subsumedBy` supTy)
      case res of
        Right _ -> pure ()
        Left err -> putDocLn (cpretty err) >> exitFailure


    (Nothing, _) -> do
      putDocLn $ vsep
        [ "Welcome to STL REPL."
        , mempty
        , "Supported syntax:"
        , indent 2 $ vsep
          [ aKeyword "type" <+> aConstructor "Name" <+> parens (aVariable "a" <+> ":" <+> aKind "Type") <+> "=" <+> aVariable "t"
          , aKeyword "#eval" <+> aVariable "t"
          , aKeyword "#check" <+> aVariable "s" <+> "<:" <+> aVariable "t"
          ]
        , mempty
        ]
      repl ">>> " $ \str -> do
        case parseStatement "<repl>" (UTF8.fromString str) of
          Left err -> putStrLn err
          Right stmt ->
            let checking =
                  checkProgram (dsStatement mainHandlers stmt (dsReturn mainHandlers Nothing)) $ \_ ->
                  case stmt of
                    (Typedef pos name _ _) -> eval pos (Fix (TGlobal pos (dsGlobalName name)))
                    _                      -> pure ()
            in runTCT checking >>= either putDocLn pure

----------------------------------------------------------------------
-- Simplistic repl machinery

repl :: String -> (String -> IO ()) -> IO ()
repl prompt f =
  runInputT defaultSettings (withInterrupt loop)
  where
    loop = handle (\Interrupt -> outputStrLn "Ctrl-C" >> loop) $ do
      input <- getInputLine prompt
      case fmap (id &&& words) input of
        Nothing -> return ()
        Just (_, []) -> loop
        Just (_, [":q"]) -> return ()
        Just (input', _) -> liftIO (f input') >> loop


{-

TExists Row
  (TRecord
      (TExtend (Label "foo") TPresent (TBase TInt)
      (TExtend (Label "bar") TPresent
         (TForall Row (TExists Presence
            (TVariant (TExtend (Label "Unit") (TRef 0) (TBase TUnit) (TRef 1)))))
      (TRef 0))))

-}
