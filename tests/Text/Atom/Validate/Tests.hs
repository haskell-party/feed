module Text.Atom.Validate.Tests
  ( atomValidateTests
  ) where

import Prelude.Compat

import Data.Text (Text)
import Data.Text.Lazy (fromStrict)

import Data.XML.Types

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import Text.Atom.Feed.Validate

import qualified Text.XML as C

atomValidateTests :: Test
atomValidateTests = testGroup "Text.Atom.Validate" [testAtomValidate]

sampleEntryText :: Text
sampleEntryText =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><entry xmlns=\"http://www.w3.org/2005/Atom\"><id>http://example.com</id><title type=\"text\">example</title><updated>2000-01-01T00:00:00Z</updated><author><name>Nobody</name></author><content type=\"xhtml\"><div xmlns=\"http://www.w3.org/1999/xhtml\">This is <b>XHTML</b> content.</div></content></entry>"

testAtomValidate :: Test
testAtomValidate = testCase "simple entry is valid" testValid
  where
    testValid :: Assertion
    testValid = do
      let document = C.toXMLDocument $ C.parseText_ C.def $ fromStrict sampleEntryText
      let entry = documentRoot document
      assertEqual "" [] $ flattenT $ validateEntry entry
