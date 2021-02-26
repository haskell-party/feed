--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Export
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.RSS1.Export
  ( xmlFeed
  , textFeed
  ) where

import Prelude.Compat

import qualified Data.Text.Util as U
import Text.DublinCore.Types
import Text.RSS1.Syntax
import Text.RSS1.Utils

import Data.List.Compat
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.XML.Compat
import Data.XML.Types as XML

qualNode :: (Maybe Text, Maybe Text) -> Text -> [XML.Node] -> XML.Element
qualNode ns n cs = Element {elementName = qualName ns n, elementAttributes = [], elementNodes = cs}

qualNode' :: (Text, Text) -> Text -> [XML.Node] -> XML.Element
qualNode' (uri, pre) = qualNode (Just uri, Just pre)

---
xmlFeed :: Feed -> XML.Element
xmlFeed f =
  (qualNode' (rdfNS, rdfPrefix) "RDF" $
   map
     NodeElement
     (concat
        [ [xmlChannel (feedChannel f)]
        , mb xmlImage (feedImage f)
        , map xmlItem (feedItems f)
        , mb xmlTextInput (feedTextInput f)
        , map xmlTopic (feedTopics f)
        , feedOther f
        ]))
        -- should we expect these to be derived by the XML pretty printer..?
    { elementAttributes =
        nub $
        mkNAttr (qualName (Nothing, Nothing) "xmlns") rss10NS :
        mkNAttr (qualName (Nothing, Just "xmlns") rdfPrefix) rdfNS :
        mkNAttr (qualName (Nothing, Just "xmlns") synPrefix) synNS :
        mkNAttr (qualName (Nothing, Just "xmlns") taxPrefix) taxNS :
        mkNAttr (qualName (Nothing, Just "xmlns") conPrefix) conNS :
        mkNAttr (qualName (Nothing, Just "xmlns") dcPrefix) dcNS : feedAttrs f
    }

textFeed :: Feed -> Maybe TL.Text
textFeed = U.renderFeed xmlFeed

xmlChannel :: Channel -> XML.Element
xmlChannel ch =
  (qualNode (Just rss10NS, Nothing) "channel" $
   map
     NodeElement
     ([ xmlLeaf (rss10NS, Nothing) "title" (channelTitle ch)
      , xmlLeaf (rss10NS, Nothing) "link" (channelLink ch)
      , xmlLeaf (rss10NS, Nothing) "description" (channelDesc ch)
      ] ++
      mb xmlTextInputURI (channelTextInputURI ch) ++
      mb xmlImageURI (channelImageURI ch) ++
      xmlItemURIs (channelItemURIs ch) ++
      map xmlDC (channelDC ch) ++
      concat
        [ mb xmlUpdatePeriod (channelUpdatePeriod ch)
        , mb xmlUpdateFreq (channelUpdateFreq ch)
        , mb (xmlLeaf (synNS, Just synPrefix) "updateBase") (channelUpdateBase ch)
        ] ++
      xmlContentItems (channelContent ch) ++ xmlTopics (channelTopics ch) ++ channelOther ch))
    { elementAttributes =
        mkNAttr (qualName' (rdfNS, rdfPrefix) "about") (channelURI ch) : channelAttrs ch
    }

xmlImageURI :: URIString -> XML.Element
xmlImageURI u = xmlEmpty (rss10NS, Nothing) "image" [mkNAttr (rdfName "resource") u]

xmlImage :: Image -> XML.Element
xmlImage i =
  (qualNode (Just rss10NS, Nothing) "image" $
   map
     NodeElement
     ([ xmlLeaf (rss10NS, Nothing) "title" (imageTitle i)
      , xmlLeaf (rss10NS, Nothing) "url" (imageURL i)
      , xmlLeaf (rss10NS, Nothing) "link" (imageLink i)
      ] ++
      map xmlDC (imageDC i) ++ imageOther i))
    {elementAttributes = mkNAttr (qualName' (rdfNS, rdfPrefix) "about") (imageURI i) : imageAttrs i}

xmlItemURIs :: [URIString] -> [XML.Element]
xmlItemURIs [] = []
xmlItemURIs xs =
  [ qualNode
      (Just rss10NS, Nothing)
      "items"
      [NodeElement (qualNode' (rdfNS, rdfPrefix) "Seq" (map toRes xs))]
  ]
  where
    toRes u = NodeElement (xmlEmpty (rdfNS, Just rdfPrefix) "li" [mkNAttr (rdfName "resource") u])

xmlTextInputURI :: URIString -> XML.Element
xmlTextInputURI u = xmlEmpty (rss10NS, Nothing) "textinput" [mkNAttr (rdfName "resource") u]

xmlTextInput :: TextInputInfo -> XML.Element
xmlTextInput ti =
  (qualNode (Just rss10NS, Nothing) "textinput" $
   map NodeElement $
   [ xmlLeaf (rss10NS, Nothing) "title" (textInputTitle ti)
   , xmlLeaf (rss10NS, Nothing) "description" (textInputDesc ti)
   , xmlLeaf (rss10NS, Nothing) "name" (textInputName ti)
   , xmlLeaf (rss10NS, Nothing) "link" (textInputLink ti)
   ] ++
   map xmlDC (textInputDC ti) ++ textInputOther ti)
    {elementAttributes = mkNAttr (rdfName "about") (textInputURI ti) : textInputAttrs ti}

xmlDC :: DCItem -> XML.Element
xmlDC dc = xmlLeaf (dcNS, Just dcPrefix) (infoToTag (dcElt dc)) (dcText dc)

xmlUpdatePeriod :: UpdatePeriod -> XML.Element
xmlUpdatePeriod u = xmlLeaf (synNS, Just synPrefix) "updatePeriod" (toStr u)
  where
    toStr ux =
      case ux of
        Update_Hourly -> "hourly"
        Update_Daily -> "daily"
        Update_Weekly -> "weekly"
        Update_Monthly -> "monthly"
        Update_Yearly -> "yearly"

xmlUpdateFreq :: Integer -> XML.Element
xmlUpdateFreq f = xmlLeaf (synNS, Just synPrefix) "updateFrequency" (T.pack $ show f)

xmlContentItems :: [ContentInfo] -> [XML.Element]
xmlContentItems [] = []
xmlContentItems xs =
  [ qualNode'
      (conNS, conPrefix)
      "items"
      [ NodeElement $
        qualNode'
          (rdfNS, rdfPrefix)
          "Bag"
          (map
             (\e -> NodeElement (qualNode' (rdfNS, rdfPrefix) "li" [NodeElement (xmlContentInfo e)]))
             xs)
      ]
  ]

xmlContentInfo :: ContentInfo -> XML.Element
xmlContentInfo ci =
  (qualNode' (conNS, conPrefix) "item" $
   map
     NodeElement
     (concat
        [ mb (rdfResource (conNS, conPrefix) "format") (contentFormat ci)
        , mb (rdfResource (conNS, conPrefix) "encoding") (contentEncoding ci)
        , mb (rdfValue []) (contentValue ci)
        ]))
    {elementAttributes = mb (mkNAttr (rdfName "about")) (contentURI ci)}

rdfResource :: (Text, Text) -> Text -> Text -> XML.Element
rdfResource (uri, pre) t v = xmlEmpty (uri, Just pre) t [mkNAttr (rdfName "resource") v]

rdfValue :: [Attr] -> Text -> XML.Element
rdfValue as s = (xmlLeaf (rdfNS, Just rdfPrefix) "value" s) {elementAttributes = as}

xmlTopics :: [URIString] -> [XML.Element]
xmlTopics [] = []
xmlTopics xs =
  [ qualNode'
      (taxNS, taxPrefix)
      "topics"
      [ NodeElement
          (qualNode'
             (rdfNS, rdfPrefix)
             "Bag"
             (map (NodeElement . rdfResource (rdfNS, rdfPrefix) "li") xs))
      ]
  ]

xmlTopic :: TaxonomyTopic -> XML.Element
xmlTopic tt =
  (qualNode' (taxNS, taxPrefix) "topic" $
   map
     NodeElement
     (xmlLeaf (rss10NS, Nothing) "link" (taxonomyLink tt) :
      mb (xmlLeaf (rss10NS, Nothing) "title") (taxonomyTitle tt) ++
      mb (xmlLeaf (rss10NS, Nothing) "description") (taxonomyDesc tt) ++
      xmlTopics (taxonomyTopics tt) ++ map xmlDC (taxonomyDC tt) ++ taxonomyOther tt))
    {elementAttributes = [mkNAttr (rdfName "about") (taxonomyURI tt)]}

xmlItem :: Item -> XML.Element
xmlItem i =
  (qualNode (Just rss10NS, Nothing) "item" $
   map
     NodeElement
     ([ xmlLeaf (rss10NS, Nothing) "title" (itemTitle i)
      , xmlLeaf (rss10NS, Nothing) "link" (itemLink i)
      ] ++
      mb (xmlLeaf (rss10NS, Nothing) "description") (itemDesc i) ++
      map xmlDC (itemDC i) ++
      xmlTopics (itemTopics i) ++ map xmlContentInfo (itemContent i) ++ itemOther i))
    {elementAttributes = mkNAttr (qualName' (rdfNS, rdfPrefix) "about") (itemURI i) : itemAttrs i}

xmlLeaf :: (Text, Maybe Text) -> Text -> Text -> XML.Element
xmlLeaf (ns, pre) tg txt =
  Element
    { elementName = qualName (Just ns, pre) tg
    , elementAttributes = []
    , elementNodes = [NodeContent $ ContentText txt]
    }

xmlEmpty :: (Text, Maybe Text) -> Text -> [Attr] -> XML.Element
xmlEmpty (uri, pre) t as = (qualNode (Just uri, pre) t []) {elementAttributes = as}

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
