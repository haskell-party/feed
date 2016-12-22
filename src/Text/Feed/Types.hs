--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Types
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Feed.Types
  ( Feed(..)
  , Item(..)
  , FeedKind(..)
  ) where

import qualified Text.RSS.Syntax as RSS
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS1.Syntax as RSS1
import qualified Text.XML.Light as XML

-- | The abstract type of feed documents. The internal representation
-- is as whatever feed variant type the document was either imported or
-- has now been translated to.
data Feed
  = AtomFeed Atom.Feed
  | RSSFeed RSS.RSS
  | RSS1Feed RSS1.Feed
    -- if we're unable to correctly the well-formed XML as a feed,
    -- keep it as an untyped document.
  | XMLFeed XML.Element
  deriving (Show)

-- | The abstract type of feed items. Like the 'Text.Feed.Types.Feed' type, the
-- representation of a value is as one of the different RSS item\/entry
-- variants.
data Item
  = AtomItem Atom.Entry
  | RSSItem RSS.RSSItem
  | RSS1Item RSS1.Item
  | XMLItem XML.Element
  deriving (Show)

-- | The kinds of feed documents supported.
data FeedKind
  = AtomKind
  | RSSKind (Maybe String) -- Nothing => default version (2.0)
  | RDFKind (Maybe String) -- Nothing => default version (1.0)
  deriving (Eq, Show)
