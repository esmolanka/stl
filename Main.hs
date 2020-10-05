{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, runModule) where

import qualified System.IO as IO
import System.Console.Haskeline
import System.Exit

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Functor.Foldable (Fix(..))
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import Data.Void

import qualified Options.Applicative as Opt

import STL
import STL.CodeGen.GenHaskell (genHaskell)
import STL.Core.Check
import STL.Core.Subsumption
import STL.Elab (dsStatement, dsReturn, dsGlobalName, dsModule, Handlers(..))
import STL.Pretty hiding (list)
import STL.Schema (genSchema, genSchema')
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

runModule :: (MonadIO m) => Handlers (ExceptT Err (ReaderT Ctx m) ()) -> FilePath -> m (Either (Doc AnsiStyle) (Program Void, Maybe Type))
runModule handlers fn = do
  str <- liftIO $ BL.readFile fn
  case parseModule fn str of
    Left err ->
      pure (Left (pretty err))
    Right modul ->
      let program = dsModule handlers modul
      in  runTCT $ checkProgram program $ \case
            Just ty -> do
              hNormalise handlers (getPosition ty) ty
              inferKind ty
              ty' <- normalise lookupGlobal ty
              pure (purifyProgram program, Just ty')
            Nothing ->
              pure (purifyProgram program, Nothing)

----------------------------------------------------------------------

data Backend = GenSchema | GenHaskell

data Mode
  = Run
  | CheckAgainst
    { _superFileName :: FilePath }
  | Compile
    { _compBackend :: Backend
    , _compOutput :: Maybe FilePath
    }

parseMode :: Opt.Parser Mode
parseMode =
  (CheckAgainst
     <$> (Opt.strOption $
            Opt.long "against" <>
            Opt.metavar "SRC" <>
            Opt.help "Source supertype .stl to check against")) <|>

  (Compile
     <$> (Opt.flag' GenSchema $
           Opt.long "schema" <>
           Opt.help "Generate a schema")
     <*> (Opt.optional $ Opt.strOption $
           Opt.long "output" <>
           Opt.short 'O' <>
           Opt.metavar "FILE" <>
           Opt.help "Output file")) <|>

    (Compile
     <$> (Opt.flag' GenHaskell $
           Opt.long "haskell" <>
           Opt.help "Generate a Haskell binding")
     <*> (Opt.optional $ Opt.strOption $
           Opt.long "output" <>
           Opt.short 'O' <>
           Opt.metavar "FILE" <>
           Opt.help "Output file")) <|>

  (pure Run)

data Config = Config
  { cfgTarget :: Maybe (FilePath, Mode)
  }

parseConfig :: Opt.Parser Config
parseConfig =
  Config
    <$> (Opt.optional $
           (,) <$> (Opt.strArgument $
                     Opt.metavar "SRC" <>
                     Opt.help "Source .stl file to process")
               <*> parseMode)

main :: IO ()
main = do
  IO.hSetEncoding IO.stdin  IO.utf8
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

  Config{..} <- Opt.execParser $
    Opt.info
      (Opt.helper <*> parseConfig)
      (Opt.fullDesc <>
       Opt.progDesc "Structural Types Langauge")

  case cfgTarget of
    Nothing ->
      runRepl

    Just (fn, Run) ->
      runModule mainHandlers fn >>= \case
        Left err -> putDocLn err >> exitFailure
        Right _ -> pure ()

    Just (sub, CheckAgainst sup) -> do
      let extractType fn = \case
            Left err -> putDocLn err >> exitFailure
            Right (_, Nothing) -> putDocLn (pretty fn <> ": does not provide a type") >> exitFailure
            Right (_, Just t) -> pure t
      subTy <- extractType sub =<< runModule silentHandlers sub
      supTy <- extractType sup =<< runModule silentHandlers sup
      case runSubsumption mempty (subTy `subsumedBy` supTy) of
        (Right _,  _) -> pure ()
        (Left err, _) -> putDocLn (cpretty err) >> exitFailure

    Just (fn, Compile GenSchema output) -> do
      let run = \case
            Left err -> putDocLn err >> exitFailure
            Right a -> pure a
      (program, _mRootTy) <- run =<< runModule silentHandlers fn
      case genSchema program of
        Left err -> putDocLn err >> exitFailure
        Right doc ->
          case output of
            Nothing -> putDocLn doc
            Just fn  -> IO.withFile fn IO.WriteMode (\hnd -> hPutDoc hnd doc)

    Just (fn, Compile GenHaskell output) -> do
      let run = \case
            Left err -> putDocLn err >> exitFailure
            Right a -> pure a
      (program, _mRootTy) <- run =<< runModule silentHandlers fn
      case genSchema' program >>= flip genHaskell Nothing of
        Left err -> putDocLn err >> exitFailure
        Right doc ->
          case output of
            Nothing -> putDocLn doc
            Just fn  -> IO.withFile fn IO.WriteMode (\hnd -> hPutDoc hnd doc)

----------------------------------------------------------------------
-- Simplistic repl machinery

runRepl :: IO ()
runRepl = do
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
  replLoop ">>> " id $ \str prog -> do
    case parseStatement "<repl>" (UTF8.fromString str) of
      Left err -> do
        putStrLn err
        pure prog
      Right stmt -> do
        let checking =
              checkProgram (prog . dsStatement mainHandlers stmt $ dsReturn mainHandlers Nothing) $ \_ ->
              case stmt of
                (Typedef pos name _ _) -> do
                  eval pos (Fix (TGlobal pos (dsGlobalName name)))
                  pure (prog . dsStatement mainHandlers stmt)
                _other ->
                  pure prog
        runTCT checking >>= \case
          Right newprog ->
            pure newprog
          Left msg -> do
            putDocLn msg
            pure prog

replLoop :: forall a. String -> a -> (String -> a -> IO a) -> IO ()
replLoop prompt initst f =
  runInputT defaultSettings (withInterrupt (loop initst)) >> pure ()
  where
    loop :: a -> InputT IO a
    loop st = handle (\Interrupt -> outputStrLn "Ctrl-C" >> loop st) $ do
      input <- getInputLine prompt
      case fmap (id &&& words) input of
        Nothing -> return st
        Just (_, []) -> loop st
        Just (_, [":q"]) -> return st
        Just (input', _) -> liftIO (f input' st) >>= loop
