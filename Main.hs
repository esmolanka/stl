{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import System.Console.Haskeline
import System.Environment

import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import STL
import STL.Check
import STL.Elab (dsType, dsStatements, dsReturn)
import STL.Pretty hiding (list)
import STL.Subsumption
import STL.Syntax (parseStatement, parseModule, typePos, Statement(..), Module(..))

----------------------------------------------------------------------
-- Utils

check :: (MonadTC m, MonadIO m) => Position -> Type -> Type -> m ()
check pos sub sup = do
  k <- inferKind sub
  k' <- inferKind sup
  sub' <- normalise lookupGlobal sub
  sup' <- normalise lookupGlobal sup

  if k == k'
    then do
      let (res, state) = runSubsumption (subsumes sub' sup')
      liftIO $ putDocLn $ nest 2 $ vsep
        [ pretty pos <> colon <+> "checking subsumption"
        , cpretty sub'
        , indent 2 "<:"
        , cpretty sup'
        , mempty
        , either cpretty (\_ -> "OK") $ res
        , mempty
        , "State:" <+> align (cpretty state)
        , mempty
        ]
    else
      liftIO $ putDocLn $ nest 2 $ vsep
        [ pretty pos <> colon <+> "checking subsumption"
        , cpretty sub'
        , indent 2 "<:"
        , cpretty sup'
        , mempty
        , "Kind mismatch:" <+> cpretty k <+> "/=" <+> cpretty k'
        , mempty
        ]

eval :: (MonadTC m, MonadIO m) => Position -> Type -> m ()
eval pos ty = do
  k   <- inferKind ty
  ty' <- normalise lookupGlobal ty
  k'  <- inferKind ty'
  liftIO $ putDocLn $ nest 2 $ vsep
     [ pretty pos <> colon <+> "normalisation"
     , cpretty ty'
     , ":" <+> cpretty k' <+>
         if k /= k' then "/=" <+> cpretty k <+> "!!!" else mempty
     , mempty
     ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fn : _) -> do
      str <- BL.readFile fn
      case parseModule fn str of
        Left err -> putStrLn err
        Right (Module _ _ _ statements mret) ->
          let program =
                dsStatements eval check statements $
                  dsReturn mret
              checking = checkProgram program $ \case
                Just ty -> eval (getPosition ty) ty
                Nothing -> pure ()
          in runTCT checking >>= either putDocLn pure

    [] -> do
      repl ">>> " $ \str -> do
        case parseStatement "<repl>" (UTF8.fromString str) of
          Left err -> putStrLn err
          Right (Normalise _ t) ->
            runTCT (eval (typePos t) (dsType t)) >>= either putDocLn pure
          Right (Subsume _ s t) ->
            runTCT (check (typePos s <> typePos t) (dsType s) (dsType t)) >>= either putDocLn pure
          Right _ ->
            putStrLn "Statement not implemented"


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
