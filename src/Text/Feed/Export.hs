--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Export
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- Convert from Feeds to XML.
--
--------------------------------------------------------------------
module Text.Feed.Export
  ( Text.Feed.Export.xmlFeed -- :: Feed -> XML.Element
  ) where

import Prelude ()
import Prelude.Compat ()

import Text.Feed.Types

import Text.Atom.Feed.Export as Atom
import Text.RSS.Export as RSS
import Text.RSS1.Export as RSS1

import Text.XML.Light as XML

-- | 'xmlFeed f' serializes a @Feed@ document into a conforming
-- XML toplevel element.
xmlFeed :: Feed -> XML.Element
xmlFeed fe =
  case fe of
    AtomFeed f -> Atom.xmlFeed f
    RSSFeed f -> RSS.xmlRSS f
    RSS1Feed f -> RSS1.xmlFeed f
    XMLFeed e -> e -- that was easy!
