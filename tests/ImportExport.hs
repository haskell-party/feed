module ImportExport
  ( importExportTests
  ) where

import Prelude.Compat

import Data.Generics (everywhere, mkT)
import Data.Text (strip)
import qualified Data.Text.Lazy.IO as T
import qualified Data.XML.Types as XML
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))
import qualified Text.XML as C

import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (readAtom)
import Text.Feed.Types (Feed)
import Text.RSS.Utils (elementToDoc)

import Paths_feed

importExportTests :: Test
importExportTests =
  testGroup "ImportExport" [testImportExport readAtom "tests/files/import_export_atom.xml"]

testImportExport :: (XML.Element -> Maybe Feed) -> FilePath -> Test
testImportExport readFeed fileName =
  testCase fileName $ do
    input <- T.readFile =<< getDataFileName fileName
    let inputXml = C.parseText_ C.def input
    let Just feed = readFeed $ C.toXMLElement $ C.documentRoot inputXml
    let Just outputXml = elementToDoc $ xmlFeed feed
    let output = C.renderText C.def outputXml
    let input' = C.renderText C.def $ stripXmlWhitespace inputXml
    input' @=? output

stripXmlWhitespace :: C.Document -> C.Document
stripXmlWhitespace = everywhere (mkT stripWhitespaceNodes)
  where
    stripWhitespaceNodes e = e {C.elementNodes = filter (not . isWhite) (C.elementNodes e)}
    isWhite (C.NodeContent t) = strip t == ""
    isWhite (C.NodeComment _) = True
    isWhite _ = False
