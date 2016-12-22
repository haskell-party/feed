module Text.RSS.Tests where

import Test.Framework (Test, mutuallyExclusive, testGroup)
import Text.RSS.Export.Tests (rssExportTests)
import Text.RSS.Import.Tests (rssImportTests)
import Test.HUnit (Assertion, assertBool)
import Test.Framework.Providers.HUnit (testCase)
import Text.Feed.Export
import Text.Feed.Import
import Text.XML.Light

import Paths_feed

rssTests :: Test
rssTests =
  testGroup
    "Text.RSS"
    [mutuallyExclusive $ testGroup "RSS" [rssExportTests, rssImportTests, testFullRss20Parse]]

testFullRss20Parse :: Test
testFullRss20Parse = testCase "parse a complete rss 2.0 file" testRss20
  where
    testRss20 :: Assertion
    testRss20 = do
      putStrLn . ppTopElement . xmlFeed =<<
        parseFeedFromFile =<< getDataFileName "tests/files/rss20.xml"
      assertBool "OK" True
