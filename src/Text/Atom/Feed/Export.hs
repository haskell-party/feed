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

import Text.XML.Light as XML
import Text.Atom.Feed

atom_prefix :: Maybe String
atom_prefix = Nothing -- Just "atom"

atom_thr_prefix :: Maybe String
atom_thr_prefix = Just "thr"

atomNS :: String
atomNS = "http://www.w3.org/2005/Atom"

atomThreadNS :: String
atomThreadNS = "http://purl.org/syndication/thread/1.0"

xmlns_atom :: Attr
xmlns_atom = Attr qn atomNS
  where
    qn =
      case atom_prefix of
        Nothing ->
          QName
          { qName = "xmlns"
          , qURI = Nothing
          , qPrefix = Nothing
          }
        Just s ->
          QName
          { qName = s
          , qURI = Nothing -- XXX: is this ok?
          , qPrefix = Just "xmlns"
          }

xmlns_atom_thread :: Attr
xmlns_atom_thread = Attr qn atomThreadNS
  where
    qn =
      case atom_prefix of
        Nothing ->
          QName
          { qName = "xmlns"
          , qURI = Nothing
          , qPrefix = Nothing
          }
        Just s ->
          QName
          { qName = s
          , qURI = Nothing -- XXX: is this ok?
          , qPrefix = Just "xmlns"
          }

atomName :: String -> QName
atomName nc =
  QName
  { qName = nc
  , qURI = Just atomNS
  , qPrefix = atom_prefix
  }

atomAttr :: String -> String -> Attr
atomAttr = Attr . atomName

atomNode :: String -> [XML.Content] -> XML.Element
atomNode x xs =
  blank_element
  { elName = atomName x
  , elContent = xs
  }

atomLeaf :: String -> String -> XML.Element
atomLeaf tag txt =
  blank_element
  { elName = atomName tag
  , elContent =
    [ Text
        blank_cdata
        { cdData = txt
        }
    ]
  }

atomThreadName :: String -> QName
atomThreadName nc =
  QName
  { qName = nc
  , qURI = Just atomThreadNS
  , qPrefix = atom_thr_prefix
  }

atomThreadAttr :: String -> String -> Attr
atomThreadAttr = Attr . atomThreadName

atomThreadNode :: String -> [XML.Content] -> XML.Element
atomThreadNode x xs =
  blank_element
  { elName = atomThreadName x
  , elContent = xs
  }

atomThreadLeaf :: String -> String -> XML.Element
atomThreadLeaf tag txt =
  blank_element
  { elName = atomThreadName tag
  , elContent =
    [ Text
        blank_cdata
        { cdData = txt
        }
    ]
  }

--------------------------------------------------------------------------------
xmlFeed :: Feed -> XML.Element
xmlFeed f =
  (atomNode "feed" . map Elem $
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
  { elAttribs = [xmlns_atom]
  }

xmlEntry :: Entry -> XML.Element
xmlEntry e =
  (atomNode "entry" . map Elem $
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
  { elAttribs = entryAttrs e
  }

xmlContent :: EntryContent -> XML.Element
xmlContent cont =
  case cont of
    TextContent t ->
      (atomLeaf "content" t)
      { elAttribs = [atomAttr "type" "text"]
      }
    HTMLContent t ->
      (atomLeaf "content" t)
      { elAttribs = [atomAttr "type" "html"]
      }
    XHTMLContent x ->
      (atomNode "content" [Elem x])
      { elAttribs = [atomAttr "type" "xhtml"]
      }
    MixedContent mbTy cs ->
      (atomNode "content" cs)
      { elAttribs = mb (atomAttr "type") mbTy
      }
    ExternalContent mbTy src ->
      (atomNode "content" [])
      { elAttribs = atomAttr "src" src : mb (atomAttr "type") mbTy
      }

xmlCategory :: Category -> XML.Element
xmlCategory c =
  (atomNode "category" (map Elem (catOther c)))
  { elAttribs =
    [atomAttr "term" (catTerm c)] ++
    mb (atomAttr "scheme") (catScheme c) ++ mb (atomAttr "label") (catLabel c)
  }

xmlLink :: Link -> XML.Element
xmlLink l =
  (atomNode "link" (map Elem (linkOther l)))
  { elAttribs =
    [atomAttr "href" (linkHref l)] ++
    mb (atomAttr "rel" . either id id) (linkRel l) ++
    mb (atomAttr "type") (linkType l) ++
    mb (atomAttr "hreflang") (linkHrefLang l) ++
    mb (atomAttr "title") (linkTitle l) ++ mb (atomAttr "length") (linkLength l) ++ linkAttrs l
  }

xmlSource :: Source -> Element
xmlSource s =
  atomNode "source" . map Elem $
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
  { elAttribs = mb (atomAttr "uri") (genURI g) ++ mb (atomAttr "version") (genVersion g)
  }

xmlAuthor :: Person -> XML.Element
xmlAuthor p = atomNode "author" (xmlPerson p)

xmlContributor :: Person -> XML.Element
xmlContributor c = atomNode "contributor" (xmlPerson c)

xmlPerson :: Person -> [XML.Content]
xmlPerson p =
  map Elem $
  [atomLeaf "name" (personName p)] ++
  mb (atomLeaf "uri") (personURI p) ++ mb (atomLeaf "email") (personEmail p) ++ personOther p

xmlInReplyTo :: InReplyTo -> XML.Element
xmlInReplyTo irt =
  (atomThreadNode "in-reply-to" (replyToContent irt))
  { elAttribs =
    mb (atomThreadAttr "ref") (Just $ replyToRef irt) ++
    mb (atomThreadAttr "href") (replyToHRef irt) ++
    mb (atomThreadAttr "type") (replyToType irt) ++
    mb (atomThreadAttr "source") (replyToSource irt) ++ replyToOther irt
  }

xmlInReplyTotal :: InReplyTotal -> XML.Element
xmlInReplyTotal irt =
  (atomThreadLeaf "total" (show $ replyToTotal irt))
  { elAttribs = replyToTotalOther irt
  }

xmlId :: String -> XML.Element
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

xmlTextContent :: String -> TextContent -> XML.Element
xmlTextContent tg t =
  case t of
    TextString s ->
      (atomLeaf tg s)
      { elAttribs = [atomAttr "type" "text"]
      }
    HTMLString s ->
      (atomLeaf tg s)
      { elAttribs = [atomAttr "type" "html"]
      }
    XHTMLString e ->
      (atomNode tg [XML.Elem e])
      { elAttribs = [atomAttr "type" "xhtml"]
      }

--------------------------------------------------------------------------------
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
