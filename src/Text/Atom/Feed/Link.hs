--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed.Link
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Atom.Feed.Link
  ( LinkRelation(..)
  , showLinkRelation
  , showLinkAttr
  ) where

-- | Atom feeds uses typed IRI links to represent
-- information \/ metadata that is of interest to the
-- consumers (software, in the main) of feeds. For instance,
-- the edit link relation attached to an atom:entry element
-- points to the IRI to use to update\/edit it.
--
-- The Atom standard encourages that such typed links to
-- be registered with IANA if they have wider applicability,
-- and the 'LinkRelation' data type encodes the currently
-- registered link types (derived from:
--  http:\/\/www.iana.org\/assignments\/link-relations.html
-- on 2007-10-28]
--
data LinkRelation -- relevant RFC:
  = LinkAlternate -- http://www.rfc-editor.org/rfc/rfc4287.txt
  | LinkCurrent -- http://www.rfc-editor.org/rfc/rfc5005.txt
  | LinkEnclosure -- http://www.rfc-editor.org/rfc/rfc4287.txt
  | LinkEdit -- http://www.rfc-editor.org/rfc/rfc5023.txt
  | LinkEditMedia -- http://www.rfc-editor.org/rfc/rfc5023.txt
  | LinkFirst -- http://www.iana.org/assignments/link-relations/first
  | LinkLast -- http://www.iana.org/assignments/link-relations/last
  | LinkLicense -- http://www.rfc-editor.org/rfc/rfc4946.txt
  | LinkNext -- http://www.rfc-editor.org/rfc/rfc5005.txt
  | LinkNextArchive -- http://www.rfc-editor.org/rfc/rfc5005.txt
  | LinkPayment -- http://www.iana.org/assignments/link-relations/payment
  | LinkPrevArchive -- http://www.rfc-editor.org/rfc/rfc5005.txt
  | LinkPrevious -- http://www.rfc-editor.org/rfc/rfc5005.txt
  | LinkRelated -- http://www.rfc-editor.org/rfc/rfc4287.txt
  | LinkReplies -- http://www.rfc-editor.org/rfc/rfc4685.txt
  | LinkSelf -- http://www.rfc-editor.org/rfc/rfc4287.txt
  | LinkVia -- http://www.rfc-editor.org/rfc/rfc4287.txt
  | LinkOther String
  deriving (Eq, Show)

showLinkRelation :: LinkRelation -> String
showLinkRelation lr =
  case lr of
    LinkAlternate -> "alternate"
    LinkCurrent -> "current"
    LinkEnclosure -> "enclosure"
    LinkEdit -> "edit"
    LinkEditMedia -> "edit-media"
    LinkFirst -> "first"
    LinkLast -> "last"
    LinkLicense -> "license"
    LinkNext -> "next"
    LinkNextArchive -> "next-archive"
    LinkPayment -> "payment"
    LinkPrevArchive -> "prev-archive"
    LinkPrevious -> "previous"
    LinkRelated -> "related"
    LinkReplies -> "replies"
    LinkSelf -> "self"
    LinkVia -> "via"
    LinkOther s -> s

showLinkAttr :: LinkRelation -> String -> String {-URI-}
showLinkAttr lr s = showLinkRelation lr ++ '=' : '"' : concatMap escQ s ++ "\""
  where
    escQ '"' = "&dquot;"
    escQ x = [x]
