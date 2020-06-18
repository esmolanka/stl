{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module STL.Runtime.Remote.Logger
  ( LogLevel (..)
  , setLogLevel
  , logStrLn
  ) where

import System.IO.Unsafe
import Control.Monad (when)
import Control.Exception (bracket)
import Control.Concurrent

data LogLevel
  = DebugLevel
  | InfoLevel
  | ErrorLevel
  deriving (Eq, Ord)

instance Show LogLevel where
  show = \case
    DebugLevel -> "DEBUG"
    InfoLevel  -> "INFO"
    ErrorLevel -> "ERROR"

loggerSem :: QSem
loggerSem = unsafePerformIO $ newQSem 1
{-# NOINLINE loggerSem #-}

logLevel :: MVar LogLevel
logLevel = unsafePerformIO $ newMVar InfoLevel
{-# NOINLINE logLevel #-}

setLogLevel :: LogLevel -> IO ()
setLogLevel !lvl =
  modifyMVarMasked_ logLevel $ \(!_) ->
    pure lvl

logStrLn :: LogLevel -> String -> IO ()
logStrLn level str = do
  bracket
    (waitQSem loggerSem)
    (\_ -> signalQSem loggerSem)
    (\_ -> do
        baseLevel <- readMVar logLevel
        when (level >= baseLevel) $
          putStrLn $ "[" ++ show level ++ "]\t" ++ str)
