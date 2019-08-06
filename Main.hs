{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main, runModule) where

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
import STL.Check
import STL.Elab (dsStatement, dsReturn, dsGlobalName, dsModule, Handlers(..))
import STL.Pretty hiding (list)
import STL.Subsumption
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
  let (res, state) = runSubsumption (sub' `subsumedBy` sup')
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

runModule :: (MonadIO m) => Handlers (ExceptT Err (ReaderT Ctx m) ()) -> FilePath -> m (Either (Doc AnsiStyle) ())
runModule handlers fn = do
  str <- liftIO $ BL.readFile fn
  case parseModule fn str of
    Left err -> pure (Left (pretty err))
    Right modul ->
      let program = dsModule handlers modul
          checking = checkProgram program $ \case
            Just ty -> eval (getPosition ty) ty
            Nothing -> pure ()
      in runTCT checking

----------------------------------------------------------------------

config :: Opt.Parser (Maybe FilePath)
config =
  Opt.optional $ Opt.strArgument $
    Opt.metavar "SRC" <>
    Opt.help "Source .stl file to process"

main :: IO ()
main = do
  input <- Opt.execParser $
    Opt.info
      (Opt.helper <*> config)
      (Opt.fullDesc <>
       Opt.progDesc "Sructurally Typed interface description Language interpreter")

  case input of
    Just fn ->
      runModule mainHandlers fn >>=
      either (\err -> putDocLn err >> exitFailure) pure

    Nothing -> do
      putDocLn $ vsep
        [ "Welcome to STL Repl"
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
                  checkProgram (dsStatement mainHandlers stmt (dsReturn Nothing)) $ \_ ->
                  case stmt of
                    (Typedef pos name _ _) -> eval pos (Fix (TGlobal pos (dsGlobalName name)))
                    _                      -> pure ()
            in runTCT checking >>= either putDocLn pure

----------------------------------------------------------------------
-- Simplistic repl machinery

repl :: String -> (String -> IO ()) -> IO ()
repl prompt f =
  runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine prompt
      case fmap (id &&& words) input of
        Nothing -> return ()
        Just (_, []) -> loop
        Just (_, [":q"]) -> return ()
        Just (input', _) -> liftIO (f input') >> loop
