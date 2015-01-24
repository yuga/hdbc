{-# LANGUAGE CPP #-}
module Database.HDBC.Locale
    (
     defaultTimeLocale,
     iso8601DateFormat
    )

where
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

-- | As the semantic of System.Locale.iso8601DateFormat has changed with
--   old-locale-1.0.0.2 in a non-compatible way, we now define our own
--   (compatible) version of it.
iso8601DateFormat :: Maybe String -> String
iso8601DateFormat mTimeFmt =
    "%Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> ' ' : fmt
