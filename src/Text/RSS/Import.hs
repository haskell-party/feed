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

import Text.RSS.Syntax
import Text.RSS1.Utils (dcNS, dcPrefix)
import Text.XML.Light as XML

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Char (isSpace)
import Control.Monad (guard, mplus)

pNodes :: String -> [XML.Element] -> [XML.Element]
pNodes x = filter ((qualName x ==) . elName)

pQNodes :: QName -> [XML.Element] -> [XML.Element]
pQNodes x = filter ((x ==) . elName)

pNode :: String -> [XML.Element] -> Maybe XML.Element
pNode x es = listToMaybe (pNodes x es)

pQNode :: QName -> [XML.Element] -> Maybe XML.Element
pQNode x es = listToMaybe (pQNodes x es)

pLeaf :: String -> [XML.Element] -> Maybe String
pLeaf x = fmap strContent . pNode x

pQLeaf :: QName -> [XML.Element] -> Maybe String
pQLeaf x = fmap strContent . pQNode x

pAttr :: String -> XML.Element -> Maybe String
pAttr x e =
  lookup
    (qualName x)
    [ (k, v)
    | Attr k v <- elAttribs e ]

pMany :: String -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es = mapMaybe f (pNodes p es)

children :: XML.Element -> [XML.Element]
children e = onlyElems (elContent e)

qualName :: String -> QName
qualName x =
  QName
  { qName = x
  , qURI = Nothing
  , qPrefix = Nothing
  }

dcName :: String -> QName
dcName x =
  blank_name
  { qName = x
  , qURI = dcNS
  , qPrefix = dcPrefix
  }

elementToRSS :: XML.Element -> Maybe RSS
elementToRSS e = do
  guard (elName e == qualName "rss")
  let es = children e
  let as = elAttribs e
  v <- pAttr "version" e
  ch <- pNode "channel" es >>= elementToChannel
  return
    RSS
    { rssVersion = v
    , rssAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    , rssChannel = ch
    , rssOther = filter (\e1 -> elName e1 /= qualName "channel") es
    }
  where
    known_attrs = ["version"]

elementToChannel :: XML.Element -> Maybe RSSChannel
elementToChannel e = do
  guard (elName e == qualName "channel")
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
    , rssChannelOther = filter (\e1 -> elName e1 `notElem` known_channel_elts) es
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
  guard (elName e == qualName "image")
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
    , rssImageOther = filter (\e1 -> elName e1 `notElem` known_image_elts) es
    }
  where
    known_image_elts = map qualName ["url", "title", "link", "width", "height", "description"]

elementToCategory :: XML.Element -> Maybe RSSCategory
elementToCategory e = do
  guard (elName e == qualName "category")
  let as = elAttribs e
  return
    RSSCategory
    { rssCategoryDomain = pAttr "domain" e
    , rssCategoryAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    , rssCategoryValue = strContent e
    }
  where
    known_attrs = ["domain"]

elementToCloud :: XML.Element -> Maybe RSSCloud
elementToCloud e = do
  guard (elName e == qualName "cloud")
  let as = elAttribs e
  return
    RSSCloud
    { rssCloudDomain = pAttr "domain" e
    , rssCloudPort = pAttr "port" e
    , rssCloudPath = pAttr "path" e
    , rssCloudRegisterProcedure = pAttr "registerProcedure" e
    , rssCloudProtocol = pAttr "protocol" e
    , rssCloudAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    }
  where
    known_attrs = ["domain", "port", "path", "registerProcedure", "protocol"]

elementToItem :: XML.Element -> Maybe RSSItem
elementToItem e = do
  guard (elName e == qualName "item")
  let es = children e
  return
    RSSItem
    { rssItemTitle = pLeaf "title" es
    , rssItemLink = pLeaf "link" es
    , rssItemDescription = pLeaf "description" es
    , rssItemAuthor = pLeaf "author" es `mplus` pQLeaf (dcName "creator") es
    , rssItemCategories = pMany "category" elementToCategory es
    , rssItemComments = pLeaf "comments" es
    , rssItemEnclosure = pNode "enclosure" es >>= elementToEnclosure
    , rssItemGuid = pNode "guid" es >>= elementToGuid
    , rssItemPubDate = pLeaf "pubDate" es `mplus` pQLeaf (dcName "date") es
    , rssItemSource = pNode "source" es >>= elementToSource
    , rssItemAttrs = elAttribs e
    , rssItemOther = filter (\e1 -> elName e1 `notElem` known_item_elts) es
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
  guard (elName e == qualName "source")
  let as = elAttribs e
  url <- pAttr "url" e
  return
    RSSSource
    { rssSourceURL = url
    , rssSourceAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    , rssSourceTitle = strContent e
    }
  where
    known_attrs = ["url"]

elementToEnclosure :: XML.Element -> Maybe RSSEnclosure
elementToEnclosure e = do
  guard (elName e == qualName "enclosure")
  let as = elAttribs e
  url <- pAttr "url" e
  ty <- pAttr "type" e
  return
    RSSEnclosure
    { rssEnclosureURL = url
    , rssEnclosureType = ty
    , rssEnclosureLength = pAttr "length" e >>= readInt
    , rssEnclosureAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    }
  where
    known_attrs = ["url", "type", "length"]

elementToGuid :: XML.Element -> Maybe RSSGuid
elementToGuid e = do
  guard (elName e == qualName "guid")
  let as = elAttribs e
  return
    RSSGuid
    { rssGuidPermanentURL = pAttr "isPermaLink" e >>= readBool
    , rssGuidAttrs = filter (\a -> qName (attrKey a) `notElem` known_attrs) as
    , rssGuidValue = strContent e
    }
  where
    known_attrs = ["isPermaLink"]

elementToTextInput :: XML.Element -> Maybe RSSTextInput
elementToTextInput e = do
  guard (elName e == qualName "textInput")
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
    , rssTextInputAttrs = elAttribs e
    , rssTextInputOther = filter (\e1 -> elName e1 `notElem` known_ti_elts) es
    }
  where
    known_ti_elts = map qualName ["title", "description", "name", "link"]

elementToSkipHours :: XML.Element -> Maybe [Integer]
elementToSkipHours e = do
  guard (elName e == qualName "skipHours")
  -- don't bother checking that this is below limit ( <= 24)
  return (pMany "hour" (readInt . strContent) (children e))

elementToSkipDays :: XML.Element -> Maybe [String]
elementToSkipDays e = do
  guard (elName e == qualName "skipDays")
  -- don't bother checking that this is below limit ( <= 7)
  return (pMany "day" (return . strContent) (children e))

----
readInt :: String -> Maybe Integer
readInt s =
  case reads s of
    ((x, _):_) -> Just x
    _ -> Nothing

readBool :: String -> Maybe Bool
readBool s =
  case dropWhile isSpace s of
    't':'r':'u':'e':_ -> Just True
    'f':'a':'l':'s':'e':_ -> Just False
    _ -> Nothing
