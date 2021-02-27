--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed.Export
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability:: portable
-- Description: Convert from Atom to XML
--
-- Convert from Atom to XML
--
--------------------------------------------------------------------
module Text.Atom.Feed.Export
  ( atom_prefix
  , atom_thr_prefix
  , atomNS
  , atomThreadNS
  , xmlns_atom
  , xmlns_atom_thread
  , atomName
  , atomAttr
  , atomNode
  , atomLeaf
  , atomThreadName
  , atomThreadAttr
  , atomThreadNode
  , atomThreadLeaf
  , xmlFeed
  , textFeed
  , xmlEntry
  , xmlContent
  , xmlCategory
  , xmlLink
  , xmlSource
  , xmlGenerator
  , xmlAuthor
  , xmlContributor
  , xmlPerson
  , xmlInReplyTo
  , xmlInReplyTotal
  , xmlId
  , xmlIcon
  , xmlLogo
  , xmlUpdated
  , xmlPublished
  , xmlRights
  , xmlTitle
  , xmlSubtitle
  , xmlSummary
  , xmlTextContent
  , mb
  ) where

import Prelude.Compat

import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Util as U
import Data.XML.Types as XML
import Text.Atom.Feed

atom_prefix :: Maybe Text
atom_prefix = Nothing -- Just "atom"

atom_thr_prefix :: Maybe Text
atom_thr_prefix = Just "thr"

atomNS :: Text
atomNS = "http://www.w3.org/2005/Atom"

atomThreadNS :: Text
atomThreadNS = "http://purl.org/syndication/thread/1.0"

blank_element :: Name -> [Node] -> XML.Element
blank_element name = XML.Element name []

xmlns_atom :: Attr
xmlns_atom = (qn, [ContentText atomNS])
  where
    qn =
      case atom_prefix of
        Nothing -> Name {nameLocalName = "xmlns", nameNamespace = Nothing, namePrefix = Nothing}
        Just s ->
          Name
            { nameLocalName = s
            , nameNamespace = Nothing -- XXX: is this ok?
            , namePrefix = Just "xmlns"
            }

xmlns_atom_thread :: Attr
xmlns_atom_thread = (qn, [ContentText atomThreadNS])
  where
    qn =
      case atom_prefix of
        Nothing -> Name {nameLocalName = "xmlns", nameNamespace = Nothing, namePrefix = Nothing}
        Just s ->
          Name
            { nameLocalName = s
            , nameNamespace = Nothing -- XXX: is this ok?
            , namePrefix = Just "xmlns"
            }

atomName :: Text -> Name
atomName nc = Name {nameLocalName = nc, nameNamespace = Just atomNS, namePrefix = atom_prefix}

atomAttr :: Text -> Text -> Attr
atomAttr x y =
  (Name {nameLocalName = x, nameNamespace = Nothing, namePrefix = atom_prefix}, [ContentText y])

atomNode :: Text -> [Node] -> XML.Element
atomNode x = blank_element (atomName x)

atomLeaf :: Text -> Text -> XML.Element
atomLeaf tag txt = blank_element (atomName tag) [NodeContent $ ContentText txt]

atomThreadName :: Text -> Name
atomThreadName nc =
  Name {nameLocalName = nc, nameNamespace = Just atomThreadNS, namePrefix = atom_thr_prefix}

atomThreadAttr :: Text -> Text -> Attr
atomThreadAttr x y = (atomThreadName x, [ContentText y])

atomThreadNode :: Text -> [Node] -> XML.Element
atomThreadNode x = blank_element (atomThreadName x)

atomThreadLeaf :: Text -> Text -> XML.Element
atomThreadLeaf tag txt = blank_element (atomThreadName tag) [NodeContent $ ContentText txt]

--------------------------------------------------------------------------------
xmlFeed :: Feed -> XML.Element
xmlFeed f =
  (atomNode "feed" $
   map NodeElement $
   [xmlTitle (feedTitle f)] ++
   [xmlId (feedId f)] ++
   [xmlUpdated (feedUpdated f)] ++
   map xmlLink (feedLinks f) ++
   map xmlAuthor (feedAuthors f) ++
   map xmlCategory (feedCategories f) ++
   map xmlContributor (feedContributors f) ++
   mb xmlGenerator (feedGenerator f) ++
   mb xmlIcon (feedIcon f) ++
   mb xmlLogo (feedLogo f) ++
   mb xmlRights (feedRights f) ++
   mb xmlSubtitle (feedSubtitle f) ++ map xmlEntry (feedEntries f) ++ feedOther f)
    {elementAttributes = [xmlns_atom]}

textFeed :: Feed -> Maybe TL.Text
textFeed = U.renderFeed xmlFeed

xmlEntry :: Entry -> XML.Element
xmlEntry e =
  (atomNode "entry" $
   map NodeElement $
   [xmlId (entryId e)] ++
   [xmlTitle (entryTitle e)] ++
   [xmlUpdated (entryUpdated e)] ++
   map xmlAuthor (entryAuthors e) ++
   map xmlCategory (entryCategories e) ++
   mb xmlContent (entryContent e) ++
   map xmlContributor (entryContributor e) ++
   map xmlLink (entryLinks e) ++
   mb xmlPublished (entryPublished e) ++
   mb xmlRights (entryRights e) ++
   mb xmlSource (entrySource e) ++
   mb xmlSummary (entrySummary e) ++
   mb xmlInReplyTo (entryInReplyTo e) ++ mb xmlInReplyTotal (entryInReplyTotal e) ++ entryOther e)
    {elementAttributes = entryAttrs e}

xmlContent :: EntryContent -> XML.Element
xmlContent cont =
  case cont of
    TextContent t -> (atomLeaf "content" t) {elementAttributes = [atomAttr "type" "text"]}
    HTMLContent t -> (atomLeaf "content" t) {elementAttributes = [atomAttr "type" "html"]}
    XHTMLContent x ->
      (atomNode "content" [NodeElement x]) {elementAttributes = [atomAttr "type" "xhtml"]}
    MixedContent mbTy cs -> (atomNode "content" cs) {elementAttributes = mb (atomAttr "type") mbTy}
    ExternalContent mbTy src ->
      (atomNode "content" []) {elementAttributes = atomAttr "src" src : mb (atomAttr "type") mbTy}

xmlCategory :: Category -> XML.Element
xmlCategory c =
  (atomNode "category" (map NodeElement (catOther c)))
    { elementAttributes =
        [atomAttr "term" (catTerm c)] ++
        mb (atomAttr "scheme") (catScheme c) ++ mb (atomAttr "label") (catLabel c)
    }

xmlLink :: Link -> XML.Element
xmlLink l =
  (atomNode "link" (map NodeElement (linkOther l)))
    { elementAttributes =
        [atomAttr "href" (linkHref l)] ++
        mb (atomAttr "rel" . either id id) (linkRel l) ++
        mb (atomAttr "type") (linkType l) ++
        mb (atomAttr "hreflang") (linkHrefLang l) ++
        mb (atomAttr "title") (linkTitle l) ++ mb (atomAttr "length") (linkLength l) ++ linkAttrs l
    }

xmlSource :: Source -> Element
xmlSource s =
  atomNode "source" $
  map NodeElement $
  sourceOther s ++
  map xmlAuthor (sourceAuthors s) ++
  map xmlCategory (sourceCategories s) ++
  mb xmlGenerator (sourceGenerator s) ++
  mb xmlIcon (sourceIcon s) ++
  mb xmlId (sourceId s) ++
  map xmlLink (sourceLinks s) ++
  mb xmlLogo (sourceLogo s) ++
  mb xmlRights (sourceRights s) ++
  mb xmlSubtitle (sourceSubtitle s) ++
  mb xmlTitle (sourceTitle s) ++ mb xmlUpdated (sourceUpdated s)

xmlGenerator :: Generator -> Element
xmlGenerator g =
  (atomLeaf "generator" (genText g))
    {elementAttributes = mb (atomAttr "uri") (genURI g) ++ mb (atomAttr "version") (genVersion g)}

xmlAuthor :: Person -> XML.Element
xmlAuthor p = atomNode "author" (xmlPerson p)

xmlContributor :: Person -> XML.Element
xmlContributor c = atomNode "contributor" (xmlPerson c)

xmlPerson :: Person -> [XML.Node]
xmlPerson p =
  map NodeElement $
  [atomLeaf "name" (personName p)] ++
  mb (atomLeaf "uri") (personURI p) ++ mb (atomLeaf "email") (personEmail p) ++ personOther p

xmlInReplyTo :: InReplyTo -> XML.Element
xmlInReplyTo irt =
  (atomThreadNode "in-reply-to" (replyToContent irt))
    { elementAttributes =
        mb (atomThreadAttr "ref") (Just $ replyToRef irt) ++
        mb (atomThreadAttr "href") (replyToHRef irt) ++
        mb (atomThreadAttr "type") (replyToType irt) ++
        mb (atomThreadAttr "source") (replyToSource irt) ++ replyToOther irt
    }

xmlInReplyTotal :: InReplyTotal -> XML.Element
xmlInReplyTotal irt =
  (atomThreadLeaf "total" (pack $ show $ replyToTotal irt))
    {elementAttributes = replyToTotalOther irt}

xmlId :: Text -> XML.Element
xmlId = atomLeaf "id"

xmlIcon :: URI -> XML.Element
xmlIcon = atomLeaf "icon"

xmlLogo :: URI -> XML.Element
xmlLogo = atomLeaf "logo"

xmlUpdated :: Date -> XML.Element
xmlUpdated = atomLeaf "updated"

xmlPublished :: Date -> XML.Element
xmlPublished = atomLeaf "published"

xmlRights :: TextContent -> XML.Element
xmlRights = xmlTextContent "rights"

xmlTitle :: TextContent -> XML.Element
xmlTitle = xmlTextContent "title"

xmlSubtitle :: TextContent -> XML.Element
xmlSubtitle = xmlTextContent "subtitle"

xmlSummary :: TextContent -> XML.Element
xmlSummary = xmlTextContent "summary"

xmlTextContent :: Text -> TextContent -> XML.Element
xmlTextContent tg t =
  case t of
    TextString s -> (atomLeaf tg s) {elementAttributes = [atomAttr "type" "text"]}
    HTMLString s -> (atomLeaf tg s) {elementAttributes = [atomAttr "type" "html"]}
    XHTMLString e ->
      (atomNode tg [XML.NodeElement e]) {elementAttributes = [atomAttr "type" "xhtml"]}

--------------------------------------------------------------------------------
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
