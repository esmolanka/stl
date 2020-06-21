{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

----------------------------------------------------------------------
--                  Do not modify. Generated code
----------------------------------------------------------------------

module API where

import qualified Prelude as X
import qualified Data.Void as X
import qualified Data.Map as X
import qualified Data.Text as X
import qualified STL.Runtime as R
import STL.Runtime ((:~>))

data Month 
  = Month'Jan
  | Month'Feb
  | Month'Mar
  | Month'Apr
  | Month'May
  | Month'Jun
  | Month'Jul
  | Month'Aug
  | Month'Sep
  | Month'Oct
  | Month'Nov
  | Month'Dec
    deriving (X.Eq, X.Show)

data DayOfWeek 
  = DayOfWeek'Mon
  | DayOfWeek'Tue
  | DayOfWeek'Wed
  | DayOfWeek'Thu
  | DayOfWeek'Fri
  | DayOfWeek'Sat
  | DayOfWeek'Sun
    deriving (X.Eq, X.Show)

newtype UnixTime = UnixTime
  { getUnixTime :: X.Int }
  deriving (X.Eq, X.Show)

newtype TimeOfDay = TimeOfDay
  { getTimeOfDay :: X.Double }
  deriving (X.Eq, X.Show)

data Date = Date
  { day :: X.Int
  , month :: Month
  , year :: X.Int
  } deriving (X.Eq, X.Show)

data API = API
  { getCurrentTime :: (() :~> UnixTime)
  , unixTimeToDate :: (UnixTime :~> Date)
  , unixTimeToTimeOfDay :: (UnixTime :~> TimeOfDay)
  , dateToDayOfWeek :: (X.Maybe (Date :~> DayOfWeek))
  } deriving (X.Eq, X.Show)

instance R.TypeOf API where
  typeOf _ =
    (R.TExists R.Row (R.TExists R.Presence (R.TRecord (R.TExtend
    (R.mkLabel "getCurrentTime") R.TPresent (R.TArrow (R.TBase R.TUnit) (R.TBase
    R.TInt)) (R.TExtend (R.mkLabel "unixTimeToDate") R.TPresent (R.TArrow
    (R.TBase R.TInt) (R.TExists R.Row (R.TRecord (R.TExtend (R.mkLabel "day")
    R.TPresent (R.TBase R.TInt) (R.TExtend (R.mkLabel "month") R.TPresent
    (R.TForall R.Row (R.TExists R.Presence (R.TExists R.Presence (R.TExists
    R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence
    (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TExists
    R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TVariant
    (R.TExtend (R.mkLabel "Jan") (R.TRef 0) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Feb") (R.TRef 1) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Mar")
    (R.TRef 2) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Apr") (R.TRef 3)
    (R.TBase R.TUnit) (R.TExtend (R.mkLabel "May") (R.TRef 4) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Jun") (R.TRef 5) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Jul") (R.TRef 6) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Aug")
    (R.TRef 7) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Sep") (R.TRef 8)
    (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Oct") (R.TRef 9) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Nov") (R.TRef 10) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Dec") (R.TRef 11) (R.TBase R.TUnit)
    (R.TRef 12))))))))))))))))))))))))))) (R.TExtend (R.mkLabel "year")
    R.TPresent (R.TBase R.TInt) (R.TRef 0))))))) (R.TExtend
    (R.mkLabel "unixTimeToTimeOfDay") R.TPresent (R.TArrow (R.TBase R.TInt)
    (R.TBase R.TFloat)) (R.TExtend (R.mkLabel "dateToDayOfWeek") (R.TRef 0)
    (R.TArrow (R.TExists R.Row (R.TRecord (R.TExtend (R.mkLabel "day")
    R.TPresent (R.TBase R.TInt) (R.TExtend (R.mkLabel "month") R.TPresent
    (R.TForall R.Row (R.TExists R.Presence (R.TExists R.Presence (R.TExists
    R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence
    (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TExists
    R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TVariant
    (R.TExtend (R.mkLabel "Jan") (R.TRef 0) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Feb") (R.TRef 1) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Mar")
    (R.TRef 2) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Apr") (R.TRef 3)
    (R.TBase R.TUnit) (R.TExtend (R.mkLabel "May") (R.TRef 4) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Jun") (R.TRef 5) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Jul") (R.TRef 6) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Aug")
    (R.TRef 7) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Sep") (R.TRef 8)
    (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Oct") (R.TRef 9) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Nov") (R.TRef 10) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Dec") (R.TRef 11) (R.TBase R.TUnit)
    (R.TRef 12))))))))))))))))))))))))))) (R.TExtend (R.mkLabel "year")
    R.TPresent (R.TBase R.TInt) (R.TRef 0)))))) (R.TForall R.Row (R.TExists
    R.Presence (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence
    (R.TExists R.Presence (R.TExists R.Presence (R.TExists R.Presence
    (R.TVariant (R.TExtend (R.mkLabel "Mon") (R.TRef 0) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Tue") (R.TRef 1) (R.TBase R.TUnit) (R.TExtend
    (R.mkLabel "Wed") (R.TRef 2) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Thu")
    (R.TRef 3) (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Fri") (R.TRef 4)
    (R.TBase R.TUnit) (R.TExtend (R.mkLabel "Sat") (R.TRef 5) (R.TBase R.TUnit)
    (R.TExtend (R.mkLabel "Sun") (R.TRef 6) (R.TBase R.TUnit)
    (R.TRef 7)))))))))))))))))) (R.TRef 1))))))))

R.deriveSerialisation X.True "Month" ''Month
R.deriveSerialisation X.True "DayOfWeek" ''DayOfWeek
R.deriveSerialisation X.False "UnixTime" ''UnixTime
R.deriveSerialisation X.False "TimeOfDay" ''TimeOfDay
R.deriveSerialisation X.False "Date" ''Date
R.deriveSerialisation X.False "API" ''API
