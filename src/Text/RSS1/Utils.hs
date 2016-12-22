--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Utils
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.RSS1.Utils
  ( pQNodes
  , pNode
  , pQNode
  , pLeaf
  , pQLeaf
  , pAttr
  , pMany
  , children
  , qualName
  , rssPrefix
  , rss10NS
  , rdfPrefix
  , rdfNS
  , synPrefix
  , synNS
  , taxPrefix
  , taxNS
  , conPrefix
  , conNS
  , dcPrefix
  , dcNS
  , rdfName
  , rssName
  , synName
  , known_rss_elts
  , known_syn_elts
  , known_dc_elts
  , known_tax_elts
  , known_con_elts
  , removeKnownElts
  , removeKnownAttrs
  ) where

import Text.XML.Light as XML
import Text.DublinCore.Types

import Data.Maybe (listToMaybe, mapMaybe)

pQNodes :: QName -> XML.Element -> [XML.Element]
pQNodes = XML.findChildren

pNode :: String -> XML.Element -> Maybe XML.Element
pNode x e = listToMaybe (pQNodes (qualName (rss10NS, Nothing) x) e)

pQNode :: QName -> XML.Element -> Maybe XML.Element
pQNode x e = listToMaybe (pQNodes x e)

pLeaf :: String -> XML.Element -> Maybe String
pLeaf x e = strContent `fmap` pQNode (qualName (rss10NS, Nothing) x) e

pQLeaf :: (Maybe String, Maybe String) -> String -> XML.Element -> Maybe String
pQLeaf ns x e = strContent `fmap` pQNode (qualName ns x) e

pAttr :: (Maybe String, Maybe String) -> String -> XML.Element -> Maybe String
pAttr ns x e =
  lookup
    (qualName ns x)
    [ (k, v)
    | Attr k v <- elAttribs e ]

pMany :: (Maybe String, Maybe String) -> String -> (XML.Element -> Maybe a) -> XML.Element -> [a]
pMany ns p f e = mapMaybe f (pQNodes (qualName ns p) e)

children :: XML.Element -> [XML.Element]
children e = onlyElems (elContent e)

qualName :: (Maybe String, Maybe String) -> String -> QName
qualName (ns, pre) x =
  QName
  { qName = x
  , qURI = ns
  , qPrefix = pre
  }

rssPrefix, rss10NS :: Maybe String
rss10NS = Just "http://purl.org/rss/1.0/"

rssPrefix = Nothing

rdfPrefix, rdfNS :: Maybe String
rdfNS = Just "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

rdfPrefix = Just "rdf"

synPrefix, synNS :: Maybe String
synNS = Just "http://purl.org/rss/1.0/modules/syndication/"

synPrefix = Just "sy"

taxPrefix, taxNS :: Maybe String
taxNS = Just "http://purl.org/rss/1.0/modules/taxonomy/"

taxPrefix = Just "taxo"

conPrefix, conNS :: Maybe String
conNS = Just "http://purl.org/rss/1.0/modules/content/"

conPrefix = Just "content"

dcPrefix, dcNS :: Maybe String
dcNS = Just "http://purl.org/dc/elements/1.1/"

dcPrefix = Just "dc"

rdfName :: String -> QName
rdfName x =
  QName
  { qName = x
  , qURI = rdfNS
  , qPrefix = rdfPrefix
  }

rssName :: String -> QName
rssName x =
  QName
  { qName = x
  , qURI = rss10NS
  , qPrefix = rssPrefix
  }

synName :: String -> QName
synName x =
  QName
  { qName = x
  , qURI = synNS
  , qPrefix = synPrefix
  }

known_rss_elts :: [QName]
known_rss_elts = map rssName ["channel", "item", "image", "textinput"]

known_syn_elts :: [QName]
known_syn_elts = map synName ["updateBase", "updateFrequency", "updatePeriod"]

known_dc_elts :: [QName]
known_dc_elts = map (qualName (dcNS, dcPrefix)) dc_element_names

known_tax_elts :: [QName]
known_tax_elts = map (qualName (taxNS, taxPrefix)) ["topic", "topics"]

known_con_elts :: [QName]
known_con_elts = map (qualName (conNS, conPrefix)) ["items", "item", "format", "encoding"]

removeKnownElts :: XML.Element -> [XML.Element]
removeKnownElts = filter (\e1 -> elName e1 `notElem` known_elts) . children
  where
    known_elts =
      concat [known_rss_elts, known_syn_elts, known_dc_elts, known_con_elts, known_tax_elts]

removeKnownAttrs :: XML.Element -> [XML.Attr]
removeKnownAttrs = filter (\a -> attrKey a `notElem` known_attrs) . elAttribs
  where
    known_attrs = map rdfName ["about"]
