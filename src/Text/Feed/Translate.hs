--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Translate
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Translating between RSS formats; work in progress.
--
module Text.Feed.Translate
  ( translateItemTo -- :: FeedKind -> Item -> Item
  , withAtomEntry -- :: (Atom.Entry -> Atom.Entry) -> Item -> Item
  , withRSSItem -- :: (RSS.RSSItem -> RSS.RSSItem) -> Item -> Item
  , withRSS1Item -- :: (RSS1.Item -> RSS1.Item) -> Item -> Item
  ) where

import Control.Arrow ((&&&))
import Text.Atom.Feed as Atom
import Text.Feed.Constructor
import Text.Feed.Types as Feed
import Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

import Data.Maybe (fromMaybe)

-- functions for performing format-specific transformations.
-- If the item isn't in the of-interest format, no transformation
-- is performed (i.e., no on-the-fly translation into the requested
-- format is performed; the caller is responsible
--
withAtomEntry :: (Atom.Entry -> Atom.Entry) -> Item -> Item
withAtomEntry f it =
  case it of
    Feed.AtomItem e -> Feed.AtomItem (f e)
    _ -> it

withRSSItem :: (RSS.RSSItem -> RSS.RSSItem) -> Item -> Item
withRSSItem f it =
  case it of
    Feed.RSSItem e -> Feed.RSSItem (f e)
    _ -> it

withRSS1Item :: (RSS1.Item -> RSS1.Item) -> Item -> Item
withRSS1Item f it =
  case it of
    Feed.RSS1Item e -> Feed.RSS1Item (f e)
    _ -> it

translateItemTo :: FeedKind -> Item -> Item
translateItemTo fk it =
  case fk of
    AtomKind -> toAtomItem it
    RSSKind v -> toRSSItem v it
    RDFKind v -> toRDFItem v it

toRSSItem :: Maybe String -> Item -> Item
toRSSItem = error "toRSSItem: unimplemented"

toRDFItem :: Maybe String -> Item -> Item
toRDFItem = error "toRDFItem: unimplemented"

toAtomItem :: Item -> Item
toAtomItem it =
  case it of
    AtomItem {} -> it
    RSS1Item {} -> error "toAtomItem: unimplemented (from RSS1 item rep.)"
    XMLItem {} -> error "toAtomItem: unimplemented (from shallow XML rep.)"
    Feed.RSSItem ri -> foldl (\oi f -> f oi) outIt pipeline_rss_atom
      where outIt =
              withAtomEntry
                (\e ->
                    e
                    { Atom.entryOther = RSS.rssItemOther ri
                    , Atom.entryAttrs = RSS.rssItemAttrs ri
                    })
                (newItem AtomKind)
            pipeline_rss_atom =
              [ mb withItemTitle (rssItemTitle ri)
              , mb withItemLink (rssItemLink ri)
              , mb withItemDescription (rssItemDescription ri)
              , mb withItemAuthor (rssItemAuthor ri)
              , ls withItemCategories (rssItemCategories ri)
              , mb withItemId' (rssItemGuid ri)
              , mb withItemCommentLink (rssItemComments ri)
              , mb withItemEnclosure' (rssItemEnclosure ri)
              , mb withItemPubDate (rssItemPubDate ri)
              ]
            withItemEnclosure' e =
              withItemEnclosure
                (rssEnclosureURL e)
                (Just $ rssEnclosureType e)
                (rssEnclosureLength e)
            withItemId' g = withItemId (fromMaybe True (rssGuidPermanentURL g)) (rssGuidValue g)
            mb _ Nothing = id
            mb f (Just v) = f v
            ls _ [] = id
            -- hack, only used for cats, so specialize:
            ls f xs = f $ map (rssCategoryValue &&& rssCategoryDomain) xs
{-
       pipeline_rss_atom =
        [ withItemTitle    (rssItemTitle ri)
        , withItemLink     (rssLink ri)
        , withDescription  (rssDescription ri)
        , \ inp -> mb (\ la -> inp{feedLanguage=...}) (rssLanguage ri)
        , \ inp -> mb (\ ed -> inp{feedAuthors=[nullPerson{personName=ed}]})
                      (rssEditor ri)
        , \ inp -> mb (\ ed -> inp{feedAuthors=[nullPerson{personName=ed}]})
                      (rssWebMaster ri)
        , \ inp -> mb (\ pu -> withPubDate)
                      (rssPubDate ri)
        , \ inp -> mb withLastUpdate
                      (rssLastUpdate ri)
        , \ inp -> withCategories (map (\c -> (RSS.rssCategoryValue c, RSS.rssCategoryDomain c))
                                       (rssCategories ri)) inp
        , \ inp -> mb withGenerator
                      (rssGenerator ri)
        , rssDocs
        , rssCloud
        , rssTTL
        , rssImage
        , rssRating
        , rssTextInput
        , rssSkipHours
        , rssSkipDays
        }
      in
-}
