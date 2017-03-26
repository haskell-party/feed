module Text.RSS.Import.Tests where

import Prelude ()
import Prelude.Compat

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.RSS.Import
import Text.RSS.Syntax
import Text.XML.Light as XML
import Text.RSS.Utils (createContent, createQName)
import Text.RSS.Equals ()

rssImportTests :: Test
rssImportTests =
  testGroup
    "Text.RSS.Import"
    [ mutuallyExclusive $
      testGroup "RSS import" [testElementToCloudIsNotCreated, testElementToCloud]
    ]

testElementToCloudIsNotCreated :: Test
testElementToCloudIsNotCreated = testCase "should not create rss cloud" notCreateRSSCloud
  where
    notCreateRSSCloud :: Assertion
    notCreateRSSCloud = do
      let notXmlCloudElement =
            XML.Element
            { elName = createQName "notCloud"
            , elAttribs = []
            , elContent = []
            , elLine = Nothing
            }
      let expected = Nothing
      assertEqual "not create rss cloud" expected (elementToCloud notXmlCloudElement)

testElementToCloud :: Test
testElementToCloud = testCase "should create rss cloud" createRSSCloud
  where
    createRSSCloud :: Assertion
    createRSSCloud = do
      let attr =
            XML.Attr
            { attrKey = createQName "attr"
            , attrVal = "text for attr"
            }
      let xmlCloudElement =
            XML.Element
            { elName = createQName "cloud"
            , elAttribs =
              [ XML.Attr
                { attrKey = createQName "domain"
                , attrVal = "domain cloud"
                }
              , XML.Attr
                { attrKey = createQName "port"
                , attrVal = "port cloud"
                }
              , XML.Attr
                { attrKey = createQName "path"
                , attrVal = "path cloud"
                }
              , XML.Attr
                { attrKey = createQName "registerProcedure"
                , attrVal = "register cloud"
                }
              , XML.Attr
                { attrKey = createQName "protocol"
                , attrVal = "protocol cloud"
                }
              , attr
              ] :: [Attr]
            , elContent = [createContent ""]
            , elLine = Nothing
            }
      let expected =
            Just
              RSSCloud
              { rssCloudDomain = Just "domain cloud"
              , rssCloudPort = Just "port cloud"
              , rssCloudPath = Just "path cloud"
              , rssCloudRegisterProcedure = Just "register cloud"
              , rssCloudProtocol = Just "protocol cloud"
              , rssCloudAttrs = [attr]
              }
      assertEqual "create rss cloud" expected (elementToCloud xmlCloudElement)
