{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module Main where

import System.Environment

import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time

import API

import STL.Runtime
import STL.Runtime.Remote
import STL.Runtime.Remote.Logger
import STL.Runtime.Remote.Sockets

exampleService :: (MessageTransport conn) => conn -> IO ()
exampleService conn = do
  serve conn $ \register -> do
    getCurrentTime <- register $ \_ () ->
      UnixTime . truncate <$> Time.getPOSIXTime

    unixTimeToDate <- register $ \_ (UnixTime n) ->
      let (yyyy, mm, dd) = Time.toGregorian . Time.utctDay . Time.posixSecondsToUTCTime $ fromIntegral n
      in pure $ Date (fromIntegral yyyy) (toEnum mm) dd

    unixTimeToTimeOfDay <- register $ \_ (UnixTime n) ->
      let tod = Time.utctDayTime . Time.posixSecondsToUTCTime $ fromIntegral n
      in pure $ TimeOfDay (fromRational (toRational tod))

    dateToDayOfWeek <- pure Nothing

    pure (API {..})

exampleClient :: (MessageTransport conn) => conn -> IO String
exampleClient conn = do
  withRemote conn $ \_ call (API {..}) -> do
    currentTime <- call getCurrentTime ()
    date <- call unixTimeToDate currentTime
    dayofweek <- case dateToDayOfWeek of
                   Nothing -> pure "Unknown"
                   Just fn -> show <$> call fn date
    pure $ show currentTime ++ " = " ++ show date ++ " = " ++ dayofweek

main :: IO ()
main = do
  args <- getArgs
  setLogLevel DebugLevel
  case args of
    ["--serve"] -> listenOnPort 3000 exampleService
    (hostname : []) -> connectTo hostname 3000 $ \conn -> exampleClient conn >>= putStrLn
    _ -> error "USAGE: ... (--serve|<hostname>)"

----------------------------------------------------------------------

instance Enum Month where
  fromEnum = \case
    Month'Jan -> 1
    Month'Feb -> 2
    Month'Mar -> 3
    Month'Apr -> 4
    Month'May -> 5
    Month'Jun -> 6
    Month'Jul -> 7
    Month'Aug -> 8
    Month'Sep -> 9
    Month'Oct -> 10
    Month'Nov -> 11
    Month'Dec -> 12
  toEnum = \case
    1 -> Month'Jan
    2 -> Month'Feb
    3 -> Month'Mar
    4 -> Month'Apr
    5 -> Month'May
    6 -> Month'Jun
    7 -> Month'Jul
    8 -> Month'Aug
    9 -> Month'Sep
    10 -> Month'Oct
    11 -> Month'Nov
    12 -> Month'Dec
    n -> error "Invalid month"
