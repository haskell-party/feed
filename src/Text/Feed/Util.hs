--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Util
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Feed.Util
  ( toFeedDateString
  , toFeedDateStringUTC
  ) where

import Prelude ()
import Prelude.Compat

import Text.Feed.Types
import Data.Time (UTCTime, formatTime)
import qualified Data.Time.Locale.Compat
import qualified System.Locale
import System.Time (ClockTime, formatCalendarTime, toUTCTime)

-- | 'toFeedDateString' translates a calendar time into
-- the format expected by the feed kind.
toFeedDateString :: FeedKind -> ClockTime -> String {-Date-}
toFeedDateString fk ct = formatCalendarTime System.Locale.defaultTimeLocale fmt ut
  where
    fmt = feedKindTimeFormat fk
    ut = toUTCTime ct

-- | 'toFeedDateStringUTC' translates a UTC time into
-- the format expected by the feed kind.
toFeedDateStringUTC :: FeedKind -> UTCTime -> String {-Date-}
toFeedDateStringUTC fk = formatTime Data.Time.Locale.Compat.defaultTimeLocale fmt
  where
    fmt = feedKindTimeFormat fk

-- | Time format expected by the feed kind.
feedKindTimeFormat :: FeedKind -> String
feedKindTimeFormat fk =
  case fk of
    AtomKind {} -> atomRdfTimeFormat
    RSSKind {} -> "%a, %d %b %Y %H:%M:%S GMT"
    RDFKind {} -> atomRdfTimeFormat
  where
    atomRdfTimeFormat = "%Y-%m-%dT%H:%M:%SZ"
