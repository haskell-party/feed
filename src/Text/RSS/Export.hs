--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Export
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Description: Convert from RSS to XML
--
--------------------------------------------------------------------
module Text.RSS.Export
  ( qualNode
  , qualName
  , xmlRSS
  , textRSS
  , xmlChannel
  , xmlItem
  , xmlSource
  , xmlEnclosure
  , xmlCategory
  , xmlGuid
  , xmlImage
  , xmlCloud
  , xmlTextInput
  , xmlSkipHours
  , xmlSkipDays
  , xmlAttr
  , xmlLeaf
  , mb
  ) where

import Prelude.Compat

import qualified Data.Text.Util as U
import Data.XML.Compat
import Data.XML.Types as XML
import Text.RSS.Syntax

import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL

qualName :: Text -> XML.Name
qualName n = Name n Nothing Nothing

qualNode :: Text -> [XML.Node] -> XML.Element
qualNode n = Element (Name n Nothing Nothing) []

---
xmlRSS :: RSS -> XML.Element
xmlRSS r =
  (qualNode "rss" $ map NodeElement (xmlChannel (rssChannel r) : rssOther r))
    {elementAttributes = mkAttr "version" (rssVersion r) : rssAttrs r}

textRSS :: RSS -> Maybe TL.Text
textRSS = U.renderFeed xmlRSS

xmlChannel :: RSSChannel -> XML.Element
xmlChannel ch =
  qualNode "channel" $
  map
    NodeElement
    ([ xmlLeaf "title" (rssTitle ch)
     , xmlLeaf "link" (rssLink ch)
     , xmlLeaf "description" (rssDescription ch)
     ] ++
     map xmlItem (rssItems ch) ++
     mb (xmlLeaf "language") (rssLanguage ch) ++
     mb (xmlLeaf "copyright") (rssCopyright ch) ++
     mb (xmlLeaf "managingEditor") (rssEditor ch) ++
     mb (xmlLeaf "webMaster") (rssWebMaster ch) ++
     mb (xmlLeaf "pubDate") (rssPubDate ch) ++
     mb (xmlLeaf "lastBuildDate") (rssLastUpdate ch) ++
     map xmlCategory (rssCategories ch) ++
     mb (xmlLeaf "generator") (rssGenerator ch) ++
     mb (xmlLeaf "docs") (rssDocs ch) ++
     mb xmlCloud (rssCloud ch) ++
     mb (xmlLeaf "ttl" . pack . show) (rssTTL ch) ++
     mb xmlImage (rssImage ch) ++
     mb (xmlLeaf "rating") (rssRating ch) ++
     mb xmlTextInput (rssTextInput ch) ++
     mb xmlSkipHours (rssSkipHours ch) ++ mb xmlSkipDays (rssSkipDays ch) ++ rssChannelOther ch)

xmlItem :: RSSItem -> XML.Element
xmlItem it =
  (qualNode "item" $
   map
     NodeElement
     (mb (xmlLeaf "title") (rssItemTitle it) ++
      mb (xmlLeaf "link") (rssItemLink it) ++
      mb (xmlLeaf "description") (rssItemDescription it) ++
      mb (xmlLeaf "author") (rssItemAuthor it) ++
      map xmlCategory (rssItemCategories it) ++
      mb (xmlLeaf "comments") (rssItemComments it) ++
      mb xmlEnclosure (rssItemEnclosure it) ++
      mb xmlGuid (rssItemGuid it) ++
      mb (xmlLeaf "pubDate") (rssItemPubDate it) ++
      mb xmlSource (rssItemSource it) ++ rssItemOther it))
    {elementAttributes = rssItemAttrs it}

xmlSource :: RSSSource -> XML.Element
xmlSource s =
  (xmlLeaf "source" (rssSourceTitle s))
    {elementAttributes = mkAttr "url" (rssSourceURL s) : rssSourceAttrs s}

xmlEnclosure :: RSSEnclosure -> XML.Element
xmlEnclosure e =
  (xmlLeaf "enclosure" "")
    { elementAttributes =
        mkAttr "url" (rssEnclosureURL e) :
        mkAttr "type" (rssEnclosureType e) :
        mb (mkAttr "length" . pack . show) (rssEnclosureLength e) ++ rssEnclosureAttrs e
    }

xmlCategory :: RSSCategory -> XML.Element
xmlCategory c =
  (xmlLeaf "category" (rssCategoryValue c))
    { elementAttributes =
        maybe id (\n -> (mkAttr "domain" n :)) (rssCategoryDomain c) (rssCategoryAttrs c)
    }

xmlGuid :: RSSGuid -> XML.Element
xmlGuid g =
  (xmlLeaf "guid" (rssGuidValue g))
    { elementAttributes =
        maybe
          id
          (\n -> (mkAttr "isPermaLink" (toBool n) :))
          (rssGuidPermanentURL g)
          (rssGuidAttrs g)
    }
  where
    toBool False = "false"
    toBool _ = "true"

xmlImage :: RSSImage -> XML.Element
xmlImage im =
  qualNode "image" $
  map
    NodeElement
    ([ xmlLeaf "url" (rssImageURL im)
     , xmlLeaf "title" (rssImageTitle im)
     , xmlLeaf "link" (rssImageLink im)
     ] ++
     mb (xmlLeaf "width" . pack . show) (rssImageWidth im) ++
     mb (xmlLeaf "height" . pack . show) (rssImageHeight im) ++
     mb (xmlLeaf "description") (rssImageDesc im) ++ rssImageOther im)

xmlCloud :: RSSCloud -> XML.Element
xmlCloud cl =
  (xmlLeaf "cloud" "")
    { elementAttributes =
        mb (mkAttr "domain") (rssCloudDomain cl) ++
        mb (mkAttr "port") (rssCloudPort cl) ++
        mb (mkAttr "path") (rssCloudPath cl) ++
        mb (mkAttr "registerProcedure") (rssCloudRegisterProcedure cl) ++
        mb (mkAttr "protocol") (rssCloudProtocol cl) ++ rssCloudAttrs cl
    }

xmlTextInput :: RSSTextInput -> XML.Element
xmlTextInput ti =
  (qualNode "textInput" $
   map
     NodeElement
     ([ xmlLeaf "title" (rssTextInputTitle ti)
      , xmlLeaf "description" (rssTextInputDesc ti)
      , xmlLeaf "name" (rssTextInputName ti)
      , xmlLeaf "link" (rssTextInputLink ti)
      ] ++
      rssTextInputOther ti))
    {elementAttributes = rssTextInputAttrs ti}

xmlSkipHours :: [Integer] -> XML.Element
xmlSkipHours hs =
  qualNode "skipHours" $ map (NodeElement . (\n -> xmlLeaf "hour" (pack $ show n))) hs

xmlSkipDays :: [Text] -> XML.Element
xmlSkipDays hs = qualNode "skipDays" $ map (NodeElement . xmlLeaf "day") hs

xmlAttr :: Text -> Text -> Attr
xmlAttr k = mkNAttr (qualName k)

xmlLeaf :: Text -> Text -> XML.Element
xmlLeaf tg txt =
  Element
    { elementAttributes = []
    , elementName = Name tg Nothing Nothing
    , elementNodes = [NodeContent (ContentText txt)]
    }

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
