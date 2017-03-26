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
  ) where

import Prelude ()
import Prelude.Compat

import Text.XML.Light as XML
import Text.RSS1.Syntax
import Text.RSS1.Utils
import Text.DublinCore.Types

import Data.List.Compat
import Data.Maybe

qualNode :: (Maybe String, Maybe String) -> String -> [XML.Content] -> XML.Element
qualNode ns n cs =
  blank_element
  { elName = qualName ns n
  , elContent = cs
  }

---
xmlFeed :: Feed -> XML.Element
xmlFeed f =
  (qualNode (rdfNS, rdfPrefix) "RDF" $
   map
     Elem
     (concat
        [ [xmlChannel (feedChannel f)]
        , mb xmlImage (feedImage f)
        , map xmlItem (feedItems f)
        , mb xmlTextInput (feedTextInput f)
        , map xmlTopic (feedTopics f)
        , feedOther f
        ]))
  -- should we expect these to be derived by the XML pretty printer..?
  { elAttribs =
    nub $
    Attr (qualName (Nothing, Nothing) "xmlns") (fromJust rss10NS) :
    Attr (qualName (Nothing, Just "xmlns") (fromJust rdfPrefix)) (fromJust rdfNS) :
    Attr (qualName (Nothing, Just "xmlns") (fromJust synPrefix)) (fromJust synNS) :
    Attr (qualName (Nothing, Just "xmlns") (fromJust taxPrefix)) (fromJust taxNS) :
    Attr (qualName (Nothing, Just "xmlns") (fromJust conPrefix)) (fromJust conNS) :
    Attr (qualName (Nothing, Just "xmlns") (fromJust dcPrefix)) (fromJust dcNS) : feedAttrs f
  }

xmlChannel :: Channel -> XML.Element
xmlChannel ch =
  (qualNode (rss10NS, Nothing) "channel" $
   map
     Elem
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
        , mb (xmlLeaf (synNS, synPrefix) "updateBase") (channelUpdateBase ch)
        ] ++
      xmlContentItems (channelContent ch) ++ xmlTopics (channelTopics ch) ++ channelOther ch))
  { elAttribs = Attr (qualName (rdfNS, rdfPrefix) "about") (channelURI ch) : channelAttrs ch
  }

xmlImageURI :: URIString -> XML.Element
xmlImageURI u = xmlEmpty (rss10NS, Nothing) "image" [Attr (rdfName "resource") u]

xmlImage :: Image -> XML.Element
xmlImage i =
  (qualNode (rss10NS, Nothing) "image" $
   map
     Elem
     ([ xmlLeaf (rss10NS, Nothing) "title" (imageTitle i)
      , xmlLeaf (rss10NS, Nothing) "url" (imageURL i)
      , xmlLeaf (rss10NS, Nothing) "link" (imageLink i)
      ] ++
      map xmlDC (imageDC i) ++ imageOther i))
  { elAttribs = Attr (qualName (rdfNS, rdfPrefix) "about") (imageURI i) : imageAttrs i
  }

xmlItemURIs :: [URIString] -> [XML.Element]
xmlItemURIs [] = []
xmlItemURIs xs =
  [qualNode (rss10NS, Nothing) "items" [Elem (qualNode (rdfNS, rdfPrefix) "Seq" (map toRes xs))]]
  where
    toRes u = Elem (xmlEmpty (rdfNS, rdfPrefix) "li" [Attr (rdfName "resource") u])

xmlTextInputURI :: URIString -> XML.Element
xmlTextInputURI u = xmlEmpty (rss10NS, Nothing) "textinput" [Attr (rdfName "resource") u]

xmlTextInput :: TextInputInfo -> XML.Element
xmlTextInput ti =
  (qualNode (rss10NS, Nothing) "textinput" . map Elem $
   [ xmlLeaf (rss10NS, Nothing) "title" (textInputTitle ti)
   , xmlLeaf (rss10NS, Nothing) "description" (textInputDesc ti)
   , xmlLeaf (rss10NS, Nothing) "name" (textInputName ti)
   , xmlLeaf (rss10NS, Nothing) "link" (textInputLink ti)
   ] ++
   map xmlDC (textInputDC ti) ++ textInputOther ti)
  { elAttribs = Attr (rdfName "about") (textInputURI ti) : textInputAttrs ti
  }

xmlDC :: DCItem -> XML.Element
xmlDC dc = xmlLeaf (dcNS, dcPrefix) (infoToTag (dcElt dc)) (dcText dc)

xmlUpdatePeriod :: UpdatePeriod -> XML.Element
xmlUpdatePeriod u = xmlLeaf (synNS, synPrefix) "updatePeriod" (toStr u)
  where
    toStr ux =
      case ux of
        Update_Hourly -> "hourly"
        Update_Daily -> "daily"
        Update_Weekly -> "weekly"
        Update_Monthly -> "monthly"
        Update_Yearly -> "yearly"

xmlUpdateFreq :: Integer -> XML.Element
xmlUpdateFreq f = xmlLeaf (synNS, synPrefix) "updateFrequency" (show f)

xmlContentItems :: [ContentInfo] -> [XML.Element]
xmlContentItems [] = []
xmlContentItems xs =
  [ qualNode
      (conNS, conPrefix)
      "items"
      [ Elem $
        qualNode
          (rdfNS, rdfPrefix)
          "Bag"
          (map (\e -> Elem (qualNode (rdfNS, rdfPrefix) "li" [Elem (xmlContentInfo e)])) xs)
      ]
  ]

xmlContentInfo :: ContentInfo -> XML.Element
xmlContentInfo ci =
  (qualNode (conNS, conPrefix) "item" $
   map
     Elem
     (concat
        [ mb (rdfResource (conNS, conPrefix) "format") (contentFormat ci)
        , mb (rdfResource (conNS, conPrefix) "encoding") (contentEncoding ci)
        , mb (rdfValue []) (contentValue ci)
        ]))
  { elAttribs = mb (Attr (rdfName "about")) (contentURI ci)
  }

rdfResource :: (Maybe String, Maybe String) -> String -> String -> XML.Element
rdfResource ns t v = xmlEmpty ns t [Attr (rdfName "resource") v]

rdfValue :: [XML.Attr] -> String -> XML.Element
rdfValue as s =
  (xmlLeaf (rdfNS, rdfPrefix) "value" s)
  { elAttribs = as
  }

xmlTopics :: [URIString] -> [XML.Element]
xmlTopics [] = []
xmlTopics xs =
  [ qualNode
      (taxNS, taxPrefix)
      "topics"
      [ Elem
          (qualNode (rdfNS, rdfPrefix) "Bag" (map (Elem . rdfResource (rdfNS, rdfPrefix) "li") xs))
      ]
  ]

xmlTopic :: TaxonomyTopic -> XML.Element
xmlTopic tt =
  (qualNode (taxNS, taxPrefix) "topic" $
   map
     Elem
     (xmlLeaf (rss10NS, Nothing) "link" (taxonomyLink tt) :
      mb (xmlLeaf (rss10NS, Nothing) "title") (taxonomyTitle tt) ++
      mb (xmlLeaf (rss10NS, Nothing) "description") (taxonomyDesc tt) ++
      xmlTopics (taxonomyTopics tt) ++ map xmlDC (taxonomyDC tt) ++ taxonomyOther tt))
  { elAttribs = [Attr (rdfName "about") (taxonomyURI tt)]
  }

xmlItem :: Item -> XML.Element
xmlItem i =
  (qualNode (rss10NS, Nothing) "item" $
   map
     Elem
     ([ xmlLeaf (rss10NS, Nothing) "title" (itemTitle i)
      , xmlLeaf (rss10NS, Nothing) "link" (itemLink i)
      ] ++
      mb (xmlLeaf (rss10NS, Nothing) "description") (itemDesc i) ++
      map xmlDC (itemDC i) ++
      xmlTopics (itemTopics i) ++ map xmlContentInfo (itemContent i) ++ itemOther i))
  { elAttribs = Attr (qualName (rdfNS, rdfPrefix) "about") (itemURI i) : itemAttrs i
  }

xmlLeaf :: (Maybe String, Maybe String) -> String -> String -> XML.Element
xmlLeaf ns tg txt =
  blank_element
  { elName = qualName ns tg
  , elContent =
    [ Text
        blank_cdata
        { cdData = txt
        }
    ]
  }

xmlEmpty :: (Maybe String, Maybe String) -> String -> [XML.Attr] -> XML.Element
xmlEmpty ns t as =
  (qualNode ns t [])
  { elAttribs = as
  }

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
