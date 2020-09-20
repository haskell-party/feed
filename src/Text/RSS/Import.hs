--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Import
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- Converting from XML to RSS
--
--------------------------------------------------------------------
module Text.RSS.Import
  ( pNodes
  , pQNodes
  , pNode
  , pQNode
  , pLeaf
  , pQLeaf
  , pAttr
  , pMany
  , children
  , qualName
  , dcName
  , elementToRSS
  , elementToChannel
  , elementToImage
  , elementToCategory
  , elementToCloud
  , elementToItem
  , elementToSource
  , elementToEnclosure
  , elementToGuid
  , elementToTextInput
  , elementToSkipHours
  , elementToSkipDays
  , readInt
  , readBool
  ) where

import Prelude.Compat

import Data.XML.Compat
import Data.XML.Types as XML

import Text.RSS.Syntax
import Text.RSS1.Utils (dcNS, dcPrefix)

import Control.Monad.Compat (guard, mplus)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text, dropWhile)
import Data.Text.Util

pNodes :: Text -> [XML.Element] -> [XML.Element]
pNodes x = filter ((qualName x ==) . elementName)

pQNodes :: Name -> [XML.Element] -> [XML.Element]
pQNodes x = filter ((x ==) . elementName)

pNode :: Text -> [XML.Element] -> Maybe XML.Element
pNode x es = listToMaybe (pNodes x es)

pQNode :: Name -> [XML.Element] -> Maybe XML.Element
pQNode x es = listToMaybe (pQNodes x es)

pLeaf :: Text -> [XML.Element] -> Maybe Text
pLeaf x es = strContent `fmap` pNode x es

pQLeaf :: Name -> [XML.Element] -> Maybe Text
pQLeaf x es = strContent `fmap` pQNode x es

pAttr :: Text -> XML.Element -> Maybe Text
pAttr x = attributeText (qualName x)

pMany :: Text -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es = mapMaybe f (pNodes p es)

children :: XML.Element -> [XML.Element]
children = elementChildren

qualName :: Text -> Name
qualName x = Name x Nothing Nothing

dcName :: Text -> Name
dcName x = Name x (Just dcNS) (Just dcPrefix)

elementToRSS :: XML.Element -> Maybe RSS
elementToRSS e = do
  guard (elementName e == qualName "rss")
  let es = children e
  let as = elementAttributes e
  v <- pAttr "version" e
  ch <- pNode "channel" es >>= elementToChannel
  return
    RSS
      { rssVersion = v
      , rssAttrs = filter ((`notElem` known_attrs) . fst) as
      , rssChannel = ch
      , rssOther = filter (\e1 -> elementName e1 /= qualName "channel") es
      }
  where
    known_attrs = ["version"]

elementToChannel :: XML.Element -> Maybe RSSChannel
elementToChannel e = do
  guard (elementName e == qualName "channel")
  let es = children e
  title <- pLeaf "title" es
  link <- pLeaf "link" es
  return
    RSSChannel
      { rssTitle = title
      , rssLink = link
        -- being liberal, <description/> is a required channel element.
      , rssDescription = fromMaybe title (pLeaf "description" es)
      , rssItems = pMany "item" elementToItem es
      , rssLanguage = pLeaf "language" es `mplus` pQLeaf (dcName "lang") es
      , rssCopyright = pLeaf "copyright" es
      , rssEditor = pLeaf "managingEditor" es `mplus` pQLeaf (dcName "creator") es
      , rssWebMaster = pLeaf "webMaster" es
      , rssPubDate = pLeaf "pubDate" es `mplus` pQLeaf (dcName "date") es
      , rssLastUpdate = pLeaf "lastBuildDate" es `mplus` pQLeaf (dcName "date") es
      , rssCategories = pMany "category" elementToCategory es
      , rssGenerator = pLeaf "generator" es `mplus` pQLeaf (dcName "source") es
      , rssDocs = pLeaf "docs" es
      , rssCloud = pNode "cloud" es >>= elementToCloud
      , rssTTL = pLeaf "ttl" es >>= readInt
      , rssImage = pNode "image" es >>= elementToImage
      , rssRating = pLeaf "rating" es
      , rssTextInput = pNode "textInput" es >>= elementToTextInput
      , rssSkipHours = pNode "skipHours" es >>= elementToSkipHours
      , rssSkipDays = pNode "skipDays" es >>= elementToSkipDays
      , rssChannelOther = filter ((`notElem` known_channel_elts) . elementName) es
      }
  where
    known_channel_elts =
      map
        qualName
        [ "title"
        , "link"
        , "description"
        , "item"
        , "language"
        , "copyright"
        , "managingEditor"
        , "webMaster"
        , "pubDate"
        , "lastBuildDate"
        , "category"
        , "generator"
        , "docs"
        , "cloud"
        , "ttl"
        , "image"
        , "rating"
        , "textInput"
        , "skipHours"
        , "skipDays"
        ]

elementToImage :: XML.Element -> Maybe RSSImage
elementToImage e = do
  guard (elementName e == qualName "image")
  let es = children e
  url <- pLeaf "url" es
  title <- pLeaf "title" es
  link <- pLeaf "link" es
  return
    RSSImage
      { rssImageURL = url
      , rssImageTitle = title
      , rssImageLink = link
      , rssImageWidth = pLeaf "width" es >>= readInt
      , rssImageHeight = pLeaf "height" es >>= readInt
      , rssImageDesc = pLeaf "description" es
      , rssImageOther = filter ((`notElem` known_image_elts) . elementName) es
      }
  where
    known_image_elts = map qualName ["url", "title", "link", "width", "height", "description"]

elementToCategory :: XML.Element -> Maybe RSSCategory
elementToCategory e = do
  guard (elementName e == qualName "category")
  let as = elementAttributes e
  return
    RSSCategory
      { rssCategoryDomain = pAttr "domain" e
      , rssCategoryAttrs = filter ((`notElem` known_attrs) . nameLocalName . attrKey) as
      , rssCategoryValue = strContent e
      }
  where
    known_attrs = ["domain"]

elementToCloud :: XML.Element -> Maybe RSSCloud
elementToCloud e = do
  guard (elementName e == qualName "cloud")
  let as = elementAttributes e
  return
    RSSCloud
      { rssCloudDomain = pAttr "domain" e
      , rssCloudPort = pAttr "port" e
      , rssCloudPath = pAttr "path" e
      , rssCloudRegisterProcedure = pAttr "registerProcedure" e
      , rssCloudProtocol = pAttr "protocol" e
      , rssCloudAttrs = filter ((`notElem` known_attrs) . nameLocalName . attrKey) as
      }
  where
    known_attrs = ["domain", "port", "path", "registerProcedure", "protocol"]

elementToItem :: XML.Element -> Maybe RSSItem
elementToItem e = do
  guard (elementName e == qualName "item")
  let es = children e
  return
    RSSItem
      { rssItemTitle = pLeaf "title" es
      , rssItemLink = pLeaf "link" es
      , rssItemDescription = pLeaf "description" es
      , rssItemAuthor = pLeaf "author" es `mplus` pQLeaf (dcName "creator") es
      , rssItemCategories = pMany "category" elementToCategory es
      , rssItemComments = pLeaf "comments" es
      , rssItemContent = pLeaf "content" es
      , rssItemEnclosure = pNode "enclosure" es >>= elementToEnclosure
      , rssItemGuid = pNode "guid" es >>= elementToGuid
      , rssItemPubDate = pLeaf "pubDate" es `mplus` pQLeaf (dcName "date") es
      , rssItemSource = pNode "source" es >>= elementToSource
      , rssItemAttrs = elementAttributes e
      , rssItemOther = filter ((`notElem` known_item_elts) . elementName) es
      }
  where
    known_item_elts =
      map
        qualName
        [ "title"
        , "link"
        , "description"
        , "author"
        , "category"
        , "comments"
        , "enclosure"
        , "guid"
        , "pubDate"
        , "source"
        ]

elementToSource :: XML.Element -> Maybe RSSSource
elementToSource e = do
  guard (elementName e == qualName "source")
  let as = elementAttributes e
  url <- pAttr "url" e
  return
    RSSSource
      { rssSourceURL = url
      , rssSourceAttrs = filter ((`notElem` known_attrs) . nameLocalName . attrKey) as
      , rssSourceTitle = strContent e
      }
  where
    known_attrs = ["url"]

elementToEnclosure :: XML.Element -> Maybe RSSEnclosure
elementToEnclosure e = do
  guard (elementName e == qualName "enclosure")
  let as = elementAttributes e
  url <- pAttr "url" e
  ty <- pAttr "type" e
  return
    RSSEnclosure
      { rssEnclosureURL = url
      , rssEnclosureType = ty
      , rssEnclosureLength = pAttr "length" e >>= readInt
      , rssEnclosureAttrs = filter ((`notElem` known_attrs) . nameLocalName . attrKey) as
      }
  where
    known_attrs = ["url", "type", "length"]

elementToGuid :: XML.Element -> Maybe RSSGuid
elementToGuid e = do
  guard (elementName e == qualName "guid")
  let as = elementAttributes e
  return
    RSSGuid
      { rssGuidPermanentURL = pAttr "isPermaLink" e >>= readBool
      , rssGuidAttrs = filter ((`notElem` known_attrs) . nameLocalName . attrKey) as
      , rssGuidValue = strContent e
      }
  where
    known_attrs = ["isPermaLink"]

elementToTextInput :: XML.Element -> Maybe RSSTextInput
elementToTextInput e = do
  guard (elementName e == qualName "textInput")
  let es = children e
  title <- pLeaf "title" es
  desc <- pLeaf "description" es
  name <- pLeaf "name" es
  link <- pLeaf "link" es
  return
    RSSTextInput
      { rssTextInputTitle = title
      , rssTextInputDesc = desc
      , rssTextInputName = name
      , rssTextInputLink = link
      , rssTextInputAttrs = elementAttributes e
      , rssTextInputOther = filter ((`notElem` known_ti_elts) . elementName) es
      }
  where
    known_ti_elts = map qualName ["title", "description", "name", "link"]

elementToSkipHours :: XML.Element -> Maybe [Integer]
elementToSkipHours e = do
  guard (elementName e == qualName "skipHours")
     -- don't bother checking that this is below limit ( <= 24)
  return (pMany "hour" (readInt . strContent) (children e))

elementToSkipDays :: XML.Element -> Maybe [Text]
elementToSkipDays e = do
  guard (elementName e == qualName "skipDays")
     -- don't bother checking that this is below limit ( <= 7)
  return (pMany "day" (return . strContent) (children e))

----
readBool :: Text -> Maybe Bool
readBool s =
  case Data.Text.dropWhile isSpace s of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
