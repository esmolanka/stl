
module Main where

import System.Environment
import System.Exit
import STL.Syntax
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  args <- getArgs
  (fn, str) <- case args of
    (fn : _) -> (,) <$> pure fn <*> BL.readFile fn
    []       -> (,) <$> pure "*stdin*" <*> BL.getContents
  case parseModule fn str of
    Left err -> die err
    Right r -> print r
