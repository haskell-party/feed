module Text.Feed.Util.Tests where

import Data.Time
import Data.Time.Clock.POSIX
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Text.Feed.Types
import Text.Feed.Util
import System.Time

feedUtilTests :: Test
feedUtilTests =
  testGroup
    "Text.Feed.Util"
    [ testGroup
        "toFeedDateString[UTC]"
        [ testToFeedDateString AtomKind (read "2016-01-07 15:33:29 UTC") "2016-01-07T15:33:29Z"
        , testToFeedDateString
            (RSSKind Nothing)
            (read "2016-01-07 15:33:29 UTC")
            "Thu, 07 Jan 2016 15:33:29 GMT"
        , testToFeedDateString
            (RDFKind Nothing)
            (read "2016-01-07 15:33:29 UTC")
            "2016-01-07T15:33:29Z"
        ]
    ]

testToFeedDateString :: FeedKind -> UTCTime -> String -> Test
testToFeedDateString kind utct expectResult = testCase (show kind) testToDateString
  where
    testToDateString :: Assertion
    testToDateString = do
      assertEqual "toFeedDateStringUTC" expectResult (toFeedDateStringUTC kind utct)
      assertEqual "toFeedDateString" expectResult (toFeedDateString kind clockTime)
    clockTime = TOD (truncate $ utcTimeToPOSIXSeconds utct) 0
