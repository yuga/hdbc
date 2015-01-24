{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

{-# LANGUAGE CPP #-}
module TestSqlValue where
import TestInfrastructure
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (Result)
import Test.QuickCheck.Tools
import qualified Test.HUnit as HU
import Database.HDBC
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format hiding (defaultTimeLocale, iso8601DateFormat, parseTime)
#else
import Data.Time.Format
#endif
import Data.Time.LocalTime
import Database.HDBC.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Maybe

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b &&
             zonedTimeZone a == zonedTimeZone b

toSql_Int :: Int -> Result
toSql_Int x = toSql x @?= SqlInt64 (fromIntegral x)

fromSql_Int :: Int -> Result
fromSql_Int x = 
    Right x @=? safeFromSql (SqlInt64 (fromIntegral x))

#if MIN_VERSION_time(1,5,0)
parseTime :: (ParseTime t, Monad m) => TimeLocale -> String -> String -> m t
parseTime = parseTimeM True
#endif

testZonedTimeStr = "1989-08-01 15:33:01 -0500"
testZonedTime :: ZonedTime
testZonedTime = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                testZonedTimeStr

testZonedTimeFracStr = "1989-08-01 15:33:01.536 -0500"
testZonedTimeFrac :: ZonedTime
testZonedTimeFrac = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    testZonedTimeFracStr

ztparsenf =
    (HU.@=?) testZonedTime (fromSql (SqlString testZonedTimeStr))
ztparsef =
    (HU.@=?) testZonedTimeFrac (fromSql (SqlString testZonedTimeFracStr))

hut msg = HU.TestLabel msg . HU.TestCase

allt = [q "toSql Int" toSql_Int,
        q "safeFromSql Int" fromSql_Int,
        hut "non-frac parse" ztparsenf,
        hut "frac parse" ztparsef]
