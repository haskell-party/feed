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
  , Text.Feed.Export.textFeed -- :: Feed -> TL.Text
  , Text.Feed.Export.textFeedWith
  ) where

import Prelude.Compat

import Text.Feed.Types

import qualified Data.Text.Util as U
import Text.Atom.Feed.Export as Atom
import Text.RSS.Export as RSS
import Text.RSS1.Export as RSS1

import qualified Data.Text.Lazy as TL
import Data.XML.Types as XML
import Text.XML (RenderSettings)

-- | 'xmlFeed f' serializes a @Feed@ document into a conforming
-- XML toplevel element.
xmlFeed :: Feed -> XML.Element
xmlFeed fe =
  case fe of
    AtomFeed f -> Atom.xmlFeed f
    RSSFeed f -> RSS.xmlRSS f
    RSS1Feed f -> RSS1.xmlFeed f
    XMLFeed e -> e -- that was easy!

textFeed :: Feed -> Maybe TL.Text
textFeed = U.renderFeed Text.Feed.Export.xmlFeed

textFeedWith :: RenderSettings -> Feed -> Maybe TL.Text
textFeedWith settings = U.renderFeedWith settings Text.Feed.Export.xmlFeed
