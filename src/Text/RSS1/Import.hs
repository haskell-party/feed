--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Import
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.RSS1.Import
  ( elementToFeed
  ) where

import Prelude ()
import Prelude.Compat

import Text.RSS1.Syntax
import Text.RSS1.Utils
import Text.XML.Light as XML
import Text.DublinCore.Types

import Data.Maybe (mapMaybe)
import Control.Monad (guard, mplus)

---
elementToFeed :: XML.Element -> Maybe Feed
elementToFeed e = do
  guard (elName e == rdfName "RDF")
  ver <- pAttr (Nothing, Nothing) "xmlns" e `mplus` rss10NS
  ch <- pNode "channel" e >>= elementToChannel
  let mbImg = pNode "image" e >>= elementToImage
  let is = maybe [] elementToItems $ pNode "items" e
  let mbTI = pNode "textinput" e >>= elementToTextInput
  let ch1 =
        ch
        { channelItemURIs = is
        }
  let its = pMany (rss10NS, Nothing) "item" elementToItem e
  let es_rest = removeKnownElts e
  let as_rest = removeKnownAttrs e
  return
    Feed
    { feedVersion = ver
    , feedChannel = ch1
    , feedImage = mbImg
    , feedItems = its
    , feedTextInput = mbTI
    , feedTopics = mapMaybe elementToTaxonomyTopic $ pQNodes (qualName (taxNS, taxPrefix) "topic") e
    , feedOther = es_rest
    , feedAttrs = as_rest
    }

elementToItems :: XML.Element -> [URIString]
elementToItems = seqLeaves

elementToTextInput :: XML.Element -> Maybe TextInputInfo
elementToTextInput e = do
  let es = children e
  uri <- pAttr (rdfNS, rdfPrefix) "about" e
  ti <- pQLeaf (rss10NS, Nothing) "title" e
  desc <- pQLeaf (rss10NS, Nothing) "description" e
  na <- pQLeaf (rss10NS, Nothing) "name" e
  li <- pQLeaf (rss10NS, Nothing) "link" e
  let dcs = mapMaybe elementToDC es
  return
    TextInputInfo
    { textInputURI = uri
    , textInputTitle = ti
    , textInputDesc = desc
    , textInputName = na
    , textInputLink = li
    , textInputDC = dcs
    , textInputOther = es
    , textInputAttrs = elAttribs e
    }

elementToItem :: XML.Element -> Maybe Item
elementToItem e = do
  guard (elName e == qualName (rss10NS, Nothing) "item")
  let es = children e
  uri <- pAttr (rdfNS, rdfPrefix) "about" e
  ti <- pQLeaf (rss10NS, Nothing) "title" e
  li <- pQLeaf (rss10NS, Nothing) "link" e
  let desc = pQLeaf (rss10NS, Nothing) "description" e
  let dcs = mapMaybe elementToDC es
  let tos = maybe [] bagLeaves $ pQNode (qualName (taxNS, taxPrefix) "topics") e
  let cs = mapMaybe elementToContent es
  let es_other = removeKnownElts e
  let as_other = removeKnownAttrs e
  return
    Item
    { itemURI = uri
    , itemTitle = ti
    , itemLink = li
    , itemDesc = desc
    , itemDC = dcs
    , itemTopics = tos
    , itemContent = cs
    , itemOther = es_other
    , itemAttrs = as_other
    }

elementToImage :: XML.Element -> Maybe Image
elementToImage e = do
  let es = children e
  let as = elAttribs e
  uri <- pAttr (rdfNS, rdfPrefix) "about" e
  ti <- pLeaf "title" e
  ur <- pLeaf "url" e
  li <- pLeaf "link" e
  let dcs = mapMaybe elementToDC es
  return
    Image
    { imageURI = uri
    , imageTitle = ti
    , imageURL = ur
    , imageLink = li
    , imageDC = dcs
    , imageOther = es
    , imageAttrs = as
    }

elementToChannel :: XML.Element -> Maybe Channel
elementToChannel e = do
  let es = children e
  uri <- pAttr (rdfNS, rdfPrefix) "about" e
  ti <- pLeaf "title" e
  li <- pLeaf "link" e
  de <- pLeaf "description" e
  let mbImg = pLeaf "image" e
  let is = maybe [] seqLeaves $ pNode "items" e
  let tinp = pLeaf "textinput" e
  let dcs = mapMaybe elementToDC es
  let tos = maybe [] bagLeaves $ pQNode (qualName (taxNS, taxPrefix) "topics") e
  let cs = mapMaybe elementToContent es
  let es_other = removeKnownElts e
  let as_other = removeKnownAttrs e
  let def_chan =
        Channel
        { channelURI = uri
        , channelTitle = ti
        , channelLink = li
        , channelDesc = de
        , channelImageURI = mbImg
        , channelItemURIs = is
        , channelTextInputURI = tinp
        , channelDC = dcs
        , channelUpdatePeriod = Nothing
        , channelUpdateFreq = Nothing
        , channelUpdateBase = Nothing
        , channelContent = cs
        , channelTopics = tos
        , channelOther = es_other
        , channelAttrs = as_other
        }
  return (addSyndication e def_chan)

addSyndication :: XML.Element -> Channel -> Channel
addSyndication e ch =
  ch
  { channelUpdatePeriod = toUpdatePeriod <$> pQLeaf (synNS, synPrefix) "updatePeriod" e
  , channelUpdateFreq = read <$> pQLeaf (synNS, synPrefix) "updateFrequency" e
  , channelUpdateBase = pQLeaf (synNS, synPrefix) "updateBase" e
  }
  where
    toUpdatePeriod x =
      case x of
        "hourly" -> Update_Hourly
        "daily" -> Update_Daily
        "weekly" -> Update_Weekly
        "monthly" -> Update_Monthly
        "yearly" -> Update_Yearly
        _ -> Update_Hourly -- ToDo: whine

elementToDC :: XML.Element -> Maybe DCItem
elementToDC e = do
  guard (qURI (elName e) == dcNS)
  let dcItem x =
        DCItem
        { dcElt = x
        , dcText = strContent e
        }
  return . dcItem $
    case qName $ elName e of
      "title" -> DC_Title
      "creator" -> DC_Creator
      "subject" -> DC_Subject
      "description" -> DC_Description
      "publisher" -> DC_Publisher
      "contributor" -> DC_Contributor
      "date" -> DC_Date
      "type" -> DC_Type
      "format" -> DC_Format
      "identifier" -> DC_Identifier
      "source" -> DC_Source
      "language" -> DC_Language
      "relation" -> DC_Relation
      "coverage" -> DC_Coverage
      "rights" -> DC_Rights
      oth -> DC_Other oth

elementToTaxonomyTopic :: XML.Element -> Maybe TaxonomyTopic
elementToTaxonomyTopic e = do
  guard (elName e == qualName (taxNS, taxPrefix) "topic")
  let es = children e
  uri <- pAttr (rdfNS, rdfPrefix) "about" e
  li <- pQLeaf (taxNS, taxPrefix) "link" e
  return
    TaxonomyTopic
    { taxonomyURI = uri
    , taxonomyLink = li
    , taxonomyTitle = pLeaf "title" e
    , taxonomyDesc = pLeaf "description" e
    , taxonomyTopics = maybe [] bagLeaves $ pQNode (qualName (taxNS, taxPrefix) "topics") e
    , taxonomyDC = mapMaybe elementToDC es
    , taxonomyOther = es
    }

elementToContent :: XML.Element -> Maybe ContentInfo
elementToContent e = do
  guard (elName e == qualName (conNS, conPrefix) "items")
  return
    ContentInfo
    { contentURI = pAttr (rdfNS, rdfPrefix) "about" e
    , contentFormat = pQLeaf (conNS, conPrefix) "format" e
    , contentEncoding = pQLeaf (conNS, conPrefix) "encoding" e
    , contentValue = pQLeaf (rdfNS, rdfPrefix) "value" e
    }

bagLeaves :: XML.Element -> [URIString]
bagLeaves be =
  mapMaybe
    (\e -> do
       guard (elName e == qualName (rdfNS, rdfPrefix) "li")
       pAttr (rdfNS, rdfPrefix) "resource" e `mplus`
         fmap strContent (pQNode (qualName (rdfNS, rdfPrefix) "li") e))
    (maybe [] children $ pQNode (qualName (rdfNS, rdfPrefix) "Bag") be)

{-
bagElements :: XML.Element -> [XML.Element]
bagElements be =
  mapMaybe
    (\ e -> do
      guard (elName e == rdfName "li")
      return e)
    (fromMaybe [] $ fmap children $ pQNode (rdfName "Bag") be)
-}
seqLeaves :: XML.Element -> [URIString]
seqLeaves se =
  mapMaybe
    (\e -> do
       guard (elName e == rdfName "li")
       return (strContent e))
    (maybe [] children $ pQNode (rdfName "Seq") se)
