{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test (main) where

import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden

import Control.Monad.Writer

import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List
import Data.Text.Lazy.Encoding
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import STL
import STL.Core.Check
import STL.Core.Subsumption
import STL.Elab (Handlers(..))
import STL.Pretty

import Main (runModule)


main :: IO ()
main = do
  files <- sort . filter (not . any (== '#')) <$> findByExtension [".stl"] "tests"
  let testCases = map mkTestCase files
  defaultMain $ testGroup "Golden" testCases


mkTestCase :: FilePath -> TestTree
mkTestCase fn =
  goldenVsStringDiff
    (takeBaseName fn)
    (\ref new -> ["diff", "-u", ref, new])
    (replaceExtension fn ".out")
    (runWriterT (runModule testHandlers fn) >>= \case
        (Left doc, msgs) -> pure $ renderBS $ vsep $ msgs ++ [doc]
        (Right (), msgs) -> pure $ renderBS $ vsep $ msgs)
  where
    renderBS :: Doc a -> B8.ByteString
    renderBS = encodeUtf8 . renderLazy . layoutPretty defaultLayoutOptions

----------------------------------------------------------------------

output :: (MonadWriter [Doc a] m) => Doc b -> m ()
output doc = tell [unAnnotate doc]

check :: (MonadTC m, MonadWriter [Doc a] m) => Position -> Type -> Type -> m ()
check pos sub sup = do
  k <- inferKind sub
  _ <- expectExactly k $ inferKind sup
  sub' <- normalise lookupGlobal sub
  sup' <- normalise lookupGlobal sup
  let (res, _) = runSubsumption mempty (sub' `subsumedBy` sup')
  output $ nest 2 $ vsep
    [ pretty pos <> colon
    , nest 2 $ "Subtype:" <> line <> cpretty sub'
    , nest 2 $ "Supertype:" <> line <> cpretty sup'
    , nest 2 $ "Result:" <> line <> either (("FAIL" <>) . (line <>) . cpretty) (\_ -> "OK") res
    ]

eval :: (MonadTC m, MonadWriter [Doc a] m) => Position -> Type -> m ()
eval pos ty = do
  k   <- inferKind ty
  ty' <- normalise lookupGlobal ty
  _   <- expectExactly k $ inferKind ty'
  output $ nest 2 $ vsep
     [ pretty pos <> colon
     , nest 2 $ "Normalised:" <> line <> cpretty ty'
     , nest 2 $ "Kind:" <> line <> cpretty k
     ]

testHandlers :: (MonadTC m, MonadWriter [Doc a] m) => Handlers (m ())
testHandlers = Handlers eval check (\_ k -> purifyProgram k)
