module Text.RSS.Export.Tests where

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.RSS.Export
import Text.RSS.Syntax
import Text.XML.Light as XML
import Text.RSS.Utils (createContent, createQName)
import Text.RSS.Equals ()

rssExportTests :: Test
rssExportTests =
  testGroup
    "Text.RSS.Export"
    [ mutuallyExclusive $
      testGroup
        "RSS export"
        [ testCreateXMLImage
        , testCreateXMLCloud
        , testCreateXMLTextInput
        , testCreateEmptyXMLSkipHours
        , testCreateXMLSkipHours
        , testCreateEmptyXMLSkipDays
        , testCreateXMLSkipDays
        , testCreateXMLAttr
        , testCreateXMLLeaf
        ]
    ]

testCreateXMLImage :: Test
testCreateXMLImage = testCase "should create image as xml" testImage
  where
    testImage :: Assertion
    testImage = do
      let other =
            XML.Element
            { elName = createQName "other"
            , elAttribs = [] :: [Attr]
            , elContent = [createContent "image other"] :: [Content]
            , elLine = Nothing
            }
      let image =
            RSSImage
            { rssImageURL = "image url"
            , rssImageTitle = "image title"
            , rssImageLink = "image link"
            , rssImageWidth = Just 100
            , rssImageHeight = Just 200
            , rssImageDesc = Just "image desc"
            , rssImageOther = [other]
            }
      let expected =
            XML.Element
            { elName = createQName "image"
            , elAttribs = [] :: [Attr]
            , elContent =
              [ Elem
                  XML.Element
                  { elName = createQName "url"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "image url"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "title"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "image title"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "link"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "image link"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "width"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "100"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "height"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "200"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "description"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "image desc"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "other"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "image other"] :: [Content]
                  , elLine = Nothing
                  }
              ] :: [Content]
            , elLine = Nothing
            }
      assertEqual "image" expected (xmlImage image)

testCreateXMLCloud :: Test
testCreateXMLCloud = testCase "should create cloud as xml" testCloud
  where
    testCloud :: Assertion
    testCloud = do
      let attr =
            XML.Attr
            { attrKey = createQName "attr"
            , attrVal = "text for attr"
            }
      let cloud =
            RSSCloud
            { rssCloudDomain = Just "domain cloud"
            , rssCloudPort = Just "port cloud"
            , rssCloudPath = Just "path cloud"
            , rssCloudRegisterProcedure = Just "register cloud"
            , rssCloudProtocol = Just "protocol cloud"
            , rssCloudAttrs = [attr]
            }
      let expected =
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
      assertEqual "cloud" expected (xmlCloud cloud)

testCreateXMLTextInput :: Test
testCreateXMLTextInput = testCase "should create text input as xml" textInput
  where
    textInput :: Assertion
    textInput = do
      let attr =
            XML.Attr
            { attrKey = createQName "attr"
            , attrVal = "text for attr"
            }
      let other =
            XML.Element
            { elName = createQName "leaf"
            , elAttribs = [] :: [Attr]
            , elContent = [createContent "text for leaf"] :: [Content]
            , elLine = Nothing
            }
      let input =
            RSSTextInput
            { rssTextInputTitle = "title"
            , rssTextInputDesc = "desc"
            , rssTextInputName = "name"
            , rssTextInputLink = "http://url.com"
            , rssTextInputAttrs = [attr]
            , rssTextInputOther = [other]
            }
      let expected =
            XML.Element
            { elName = createQName "textInput"
            , elAttribs = [attr] :: [Attr]
            , elContent =
              [ Elem
                  XML.Element
                  { elName = createQName "title"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "title"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "description"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "desc"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "name"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "name"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "link"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "http://url.com"] :: [Content]
                  , elLine = Nothing
                  }
              , Elem
                  XML.Element
                  { elName = createQName "leaf"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent "text for leaf"] :: [Content]
                  , elLine = Nothing
                  }
              ] :: [Content]
            , elLine = Nothing
            }
      assertEqual "text input" expected (xmlTextInput input)

testCreateEmptyXMLSkipHours :: Test
testCreateEmptyXMLSkipHours = testCase "should create an empty list of skip hours as xml" emptySkipHours
  where
    emptySkipHours :: Assertion
    emptySkipHours = do
      let hoursToSkip = []
      let expected =
            XML.Element
            { elName = createQName "skipHours"
            , elAttribs = [] :: [Attr]
            , elContent = [] :: [Content]
            , elLine = Nothing
            }
      assertEqual "empty skip hours" expected (xmlSkipHours hoursToSkip)

testCreateXMLSkipHours :: Test
testCreateXMLSkipHours = testCase "should create skip hours as xml" skipHours
  where
    skipHours :: Assertion
    skipHours = do
      let hoursToSkip = [1, 2, 3]
      let expected =
            XML.Element
            { elName = createQName "skipHours"
            , elAttribs = [] :: [Attr]
            , elContent = [hourElem 0, hourElem 1, hourElem 2] :: [Content]
            , elLine = Nothing
            }
            where
              hourElem ind =
                Elem
                  XML.Element
                  { elName = createQName "hour"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent . show $ hoursToSkip !! ind]
                  , elLine = Nothing
                  }
      assertEqual "skip hours" expected (xmlSkipHours hoursToSkip)

testCreateEmptyXMLSkipDays :: Test
testCreateEmptyXMLSkipDays = testCase "should create an empty list of skip days as xml" emptySkipDays
  where
    emptySkipDays :: Assertion
    emptySkipDays = do
      let daysToSkip = []
      let expected =
            XML.Element
            { elName = createQName "skipDays"
            , elAttribs = [] :: [Attr]
            , elContent = [] :: [Content]
            , elLine = Nothing
            }
      assertEqual "empty skip days" expected (xmlSkipDays daysToSkip)

testCreateXMLSkipDays :: Test
testCreateXMLSkipDays = testCase "should create skip days as xml" skipDays
  where
    skipDays :: Assertion
    skipDays = do
      let daysToSkip = ["first day", "second day", "third day"]
      let expected =
            XML.Element
            { elName = createQName "skipDays"
            , elAttribs = [] :: [Attr]
            , elContent = [dayElem 0, dayElem 1, dayElem 2] :: [Content]
            , elLine = Nothing
            }
            where
              dayElem ind =
                Elem
                  XML.Element
                  { elName = createQName "day"
                  , elAttribs = [] :: [Attr]
                  , elContent = [createContent $ daysToSkip !! ind]
                  , elLine = Nothing
                  }
      assertEqual "skip days" expected (xmlSkipDays daysToSkip)

testCreateXMLAttr :: Test
testCreateXMLAttr = testCase "should create attr as xml" createXMLAttr
  where
    createXMLAttr :: Assertion
    createXMLAttr = do
      let tg = "attr"
      let txt = "example of attr value"
      let expected =
            XML.Attr
            { attrKey = createQName tg
            , attrVal = txt
            }
      assertEqual "create a leaf" expected (xmlAttr tg txt)

testCreateXMLLeaf :: Test
testCreateXMLLeaf = testCase "should create leaf as xml" createXMLLeaf
  where
    createXMLLeaf :: Assertion
    createXMLLeaf = do
      let tg = "leaf"
      let txt = "example of leaf text"
      let expected =
            XML.Element
            { elName = createQName tg
            , elAttribs = [] :: [Attr]
            , elContent = [createContent txt] :: [Content]
            , elLine = Nothing
            }
      assertEqual "create a leaf" expected (xmlLeaf tg txt)
