--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Constructor
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Description: Module for an abstraction layer between different kinds of feeds.
--
--------------------------------------------------------------------
module Text.Feed.Constructor
  ( FeedKind(..)
  , newFeed -- :: FeedKind  -> Feed
  , feedFromRSS -- :: RSS       -> Feed
  , feedFromAtom -- :: Atom.Feed -> Feed
  , feedFromRDF -- :: RSS1.Feed -> Feed
  , feedFromXML -- :: Element   -> Feed
  , getFeedKind -- :: Feed      -> FeedKind
  , FeedSetter -- type _ a = a -> Feed -> Feed
  , addItem -- :: FeedSetter Item
  , withFeedTitle -- :: FeedSetter String
  , withFeedHome -- :: FeedSetter URLString
  , withFeedHTML -- :: FeedSetter URLString
  , withFeedDescription -- :: FeedSetter String
  , withFeedPubDate -- :: FeedSetter DateString
  , withFeedLastUpdate -- :: FeedSetter DateString
  , withFeedDate -- :: FeedSetter DateString
  , withFeedLogoLink -- :: FeedSetter URLString
  , withFeedLanguage -- :: FeedSetter String
  , withFeedCategories -- :: FeedSetter [(String,Maybe String)]
  , withFeedGenerator -- :: FeedSetter String
  , withFeedItems -- :: FeedSetter [Item]
  , newItem -- :: FeedKind   -> Item
  , getItemKind -- :: Item       -> FeedKind
  , atomEntryToItem -- :: Atom.Entry -> Item
  , rssItemToItem -- :: RSS.Item   -> Item
  , rdfItemToItem -- :: RSS1.Item  -> Item
  , ItemSetter -- type _ a = a -> Item -> Item
  , withItemTitle -- :: ItemSetter String
  , withItemLink -- :: ItemSetter URLString
  , withItemPubDate -- :: ItemSetter DateString
  , withItemDate -- :: ItemSetter DateString
  , withItemAuthor -- :: ItemSetter String
  , withItemCommentLink -- :: ItemSetter String
  , withItemEnclosure -- :: String -> Maybe String -> ItemSetter Integer
  , withItemFeedLink -- :: String -> ItemSetter String
  , withItemId -- :: Bool   -> ItemSetter String
  , withItemCategories -- :: ItemSetter [(String, Maybe String)]
  , withItemDescription -- :: ItemSetter String
  , withItemRights -- :: ItemSetter String
  ) where

import Prelude ()
import Prelude.Compat

import Text.Feed.Types as Feed.Types

import Text.Atom.Feed as Atom
import Text.RSS.Syntax as RSS
import Text.RSS1.Syntax as RSS1
import Text.DublinCore.Types
import Text.XML.Light as XML hiding (filterChildren)

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (toLower)

-- ToDo:
--
--  - complete set of constructors over feeds
--  - provide a unified treatment of date string reps.
--    (i.e., I know they differ across formats, but ignorant what
--    the constraints are at the moment.)
-- | Construct an empty feed document, intending to output it in
-- the 'fk' feed format.
newFeed :: FeedKind -> Feed.Types.Feed
newFeed fk =
  case fk of
    AtomKind ->
      AtomFeed
        (Atom.nullFeed
           "feed-id-not-filled-in"
           (TextString "dummy-title")
           "dummy-and-bogus-update-date")
    RSSKind mbV ->
      let def = RSS.nullRSS "dummy-title" "default-channel-url"
      in RSSFeed $
         maybe
           def
           (\v ->
               def
               { RSS.rssVersion = v
               })
           mbV
    RDFKind mbV ->
      let def = RSS1.nullFeed "default-channel-url" "dummy-title"
      in RSS1Feed $
         maybe
           def
           (\v ->
               def
               { RSS1.feedVersion = v
               })
           mbV

feedFromRSS :: RSS.RSS -> Feed.Types.Feed
feedFromRSS = RSSFeed

feedFromAtom :: Atom.Feed -> Feed.Types.Feed
feedFromAtom = AtomFeed

feedFromRDF :: RSS1.Feed -> Feed.Types.Feed
feedFromRDF = RSS1Feed

feedFromXML :: XML.Element -> Feed.Types.Feed
feedFromXML = XMLFeed

getFeedKind :: Feed.Types.Feed -> FeedKind
getFeedKind f =
  case f of
    Feed.Types.AtomFeed {} -> AtomKind
    Feed.Types.RSSFeed r ->
      RSSKind
        (case RSS.rssVersion r of
           "2.0" -> Nothing
           v -> Just v)
    Feed.Types.RSS1Feed r ->
      RDFKind
        (case RSS1.feedVersion r of
           "1.0" -> Nothing
           v -> Just v)
    Feed.Types.XMLFeed {} -> RSSKind (Just "2.0") -- for now, just a hunch..

addItem :: Feed.Types.Item -> Feed.Types.Feed -> Feed.Types.Feed
addItem it f =
  case (it, f) of
    (Feed.Types.AtomItem e, Feed.Types.AtomFeed fe) ->
      Feed.Types.AtomFeed
        fe
        { Atom.feedEntries = e : Atom.feedEntries fe
        }
    (Feed.Types.RSSItem e, Feed.Types.RSSFeed r) ->
      Feed.Types.RSSFeed
        r
        { RSS.rssChannel =
          (RSS.rssChannel r)
          { RSS.rssItems = e : RSS.rssItems (RSS.rssChannel r)
          }
        }
    (Feed.Types.RSS1Item e, Feed.Types.RSS1Feed r)
    -- note: do not update the channel item URIs at this point;
    -- will delay doing so until serialization.
     ->
      Feed.Types.RSS1Feed
        r
        { RSS1.feedItems = e : RSS1.feedItems r
        }
    _ ->
      error "addItem: currently unable to automatically convert items from one feed type to another"

withFeedItems :: FeedSetter [Feed.Types.Item]
withFeedItems is fe =
  foldr
    addItem
    (case fe of
       Feed.Types.AtomFeed f ->
         Feed.Types.AtomFeed
           f
           { Atom.feedEntries = []
           }
       Feed.Types.RSSFeed f ->
         Feed.Types.RSSFeed
           f
           { rssChannel =
             (rssChannel f)
             { rssItems = []
             }
           }
       Feed.Types.RSS1Feed f ->
         Feed.Types.RSS1Feed
           f
           { feedItems = []
           })
    is

newItem :: FeedKind -> Feed.Types.Item
newItem fk =
  case fk of
    AtomKind ->
      Feed.Types.AtomItem $
      Atom.nullEntry
        "entry-id-not-filled-in"
        (TextString "dummy-entry-title")
        "dummy-and-bogus-entry-update-date"
    RSSKind {} -> Feed.Types.RSSItem $ RSS.nullItem "dummy-rss-item-title"
    RDFKind {} ->
      Feed.Types.RSS1Item $ RSS1.nullItem "dummy-item-uri" "dummy-item-title" "dummy-item-link"

getItemKind :: Feed.Types.Item -> FeedKind
getItemKind f =
  case f of
    Feed.Types.AtomItem {} -> AtomKind
    Feed.Types.RSSItem {} -> RSSKind (Just "2.0") -- good guess..
    Feed.Types.RSS1Item {} -> RDFKind (Just "1.0")
    Feed.Types.XMLItem {} -> RSSKind (Just "2.0")

type FeedSetter a = a -> Feed.Types.Feed -> Feed.Types.Feed

withFeedTitle :: FeedSetter String
withFeedTitle tit fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedTitle = TextString tit
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssTitle = tit
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed
        f
        { feedChannel =
          (feedChannel f)
          { channelTitle = tit
          }
        }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "title"
                              then Just (unode "title" tit)
                              else Nothing)
                        e)
              else Nothing)
        f

withFeedHome :: FeedSetter URLString
withFeedHome url fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedLinks = newSelf : Atom.feedLinks f
        }
    -- ToDo: fix, the <link> element is for the HTML home of the channel, not the
    -- location of the feed itself. Struggling to find if there is a common way
    -- to represent this outside of RSS 2.0 standard elements..
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssLink = url
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed
        f
        { feedChannel =
          (feedChannel f)
          { channelURI = url
          }
        }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "link"
                              then Just (unode "link" url)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    newSelf =
      (nullLink url)
      { linkRel = Just (Left "self")
      , linkType = Just "application/atom+xml"
      }

-- | 'withFeedHTML' sets the URL where an HTML version of the
-- feed is published.
withFeedHTML :: FeedSetter URLString
withFeedHTML url fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedLinks = newAlt : Atom.feedLinks f
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssLink = url
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed
        f
        { feedChannel =
          (feedChannel f)
          { channelLink = url
          }
        }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "link"
                              then Just (unode "link" url)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    newAlt =
      (nullLink url)
      { linkRel = Just (Left "alternate")
      , linkType = Just "text/html"
      }

-- | 'withFeedHTML' sets the URL where an HTML version of the
-- feed is published.
withFeedDescription :: FeedSetter String
withFeedDescription desc fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedSubtitle = Just (TextString desc)
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssDescription = desc
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed
        f
        { feedChannel =
          (feedChannel f)
          { channelDesc = desc
          }
        }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "description"
                              then Just (unode "description" desc)
                              else Nothing)
                        e)
              else Nothing)
        f

withFeedPubDate :: FeedSetter String
withFeedPubDate dateStr fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedUpdated = dateStr
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssPubDate = Just dateStr
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed $
      case break isDate $ RSS1.channelDC (RSS1.feedChannel f) of
        (as, dci:bs) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              as ++
              dci
              { dcText = dateStr
              } :
              bs
            }
          }
        (_, []) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              DCItem
              { dcElt = DC_Date
              , dcText = dateStr
              } :
              RSS1.channelDC (RSS1.feedChannel f)
            }
          }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "pubDate"
                              then Just (unode "pubDate" dateStr)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    isDate dc = dcElt dc == DC_Date

withFeedLastUpdate :: FeedSetter DateString
withFeedLastUpdate dateStr fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedUpdated = dateStr
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssLastUpdate = Just dateStr
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed $
      case break isDate $ RSS1.channelDC (RSS1.feedChannel f) of
        (as, dci:bs) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              as ++
              dci
              { dcText = dateStr
              } :
              bs
            }
          }
        (_, []) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              DCItem
              { dcElt = DC_Date
              , dcText = dateStr
              } :
              RSS1.channelDC (RSS1.feedChannel f)
            }
          }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "lastUpdate"
                              then Just (unode "lastUpdate" dateStr)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    isDate dc = dcElt dc == DC_Date

-- | 'withFeedDate dt' is the composition of 'withFeedPubDate'
-- and 'withFeedLastUpdate', setting both publication date and
-- last update date to 'dt'. Notice that RSS2.0 is the only format
-- supporting both pub and last-update.
withFeedDate :: FeedSetter DateString
withFeedDate dt f = withFeedPubDate dt (withFeedLastUpdate dt f)

withFeedLogoLink :: URLString -> FeedSetter URLString
withFeedLogoLink imgURL lnk fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { feedLogo = Just imgURL
        , feedLinks = newSelf : Atom.feedLinks f
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssImage = Just $ RSS.nullImage imgURL (rssTitle (rssChannel f)) lnk
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed $
      f
      { feedImage = Just $ RSS1.nullImage imgURL (RSS1.channelTitle (RSS1.feedChannel f)) lnk
      , feedChannel =
        (feedChannel f)
        { channelImageURI = Just imgURL
        }
      }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "image"
                              then Just
                                     (unode
                                        "image"
                                        [unode "url" imgURL, unode "title" title, unode "link" lnk])
                              else Nothing)
                        e)
              else Nothing)
        f
      where title =
              case fmap (findChild (unqual "title")) (findChild (unqual "channel") f) of
                Just (Just e1) -> strContent e1
                _ -> "feed_title" -- shouldn't happen..
  where
    newSelf =
      (nullLink lnk)
      { linkRel = Just (Left "self")
      , linkType = Just "application/atom+xml"
      }

withFeedLanguage :: FeedSetter String
withFeedLanguage lang fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed $
      f
      { Atom.feedAttrs =
        XML.Attr
          (unqual "lang")
          { qPrefix = Just "xml"
          }
          lang :
        Atom.feedAttrs f
      }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssLanguage = Just lang
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed $
      case break isLang $ RSS1.channelDC (RSS1.feedChannel f) of
        (as, dci:bs) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              as ++
              dci
              { dcText = lang
              } :
              bs
            }
          }
        (_, []) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              DCItem
              { dcElt = DC_Language
              , dcText = lang
              } :
              RSS1.channelDC (RSS1.feedChannel f)
            }
          }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "language"
                              then Just (unode "language" lang)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    isLang dc = dcElt dc == DC_Language

withFeedCategories :: FeedSetter [(String, Maybe String)]
withFeedCategories cats fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed
        f
        { Atom.feedCategories =
          map
            (\(t, mb) ->
                (Atom.newCategory t)
                { Atom.catScheme = mb
                })
            cats ++
          feedCategories f
        }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { RSS.rssCategories =
            map
              (\(t, mb) ->
                  (RSS.newCategory t)
                  { RSS.rssCategoryDomain = mb
                  })
              cats ++
            RSS.rssCategories (rssChannel f)
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed
        f
        { feedChannel =
          (feedChannel f)
          { RSS1.channelDC =
            map
              (\(t, _) ->
                  DCItem
                  { dcElt = DC_Subject
                  , dcText = t
                  })
              cats ++
            RSS1.channelDC (feedChannel f)
          }
        }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (foldr
                        (\(t, mb) acc ->
                            addChild
                              (unode
                                 "category"
                                 (maybe (: []) (\v x -> [Attr (unqual "domain") v, x]) mb $
                                  Attr (unqual "term") t))
                              acc)
                        e
                        cats)
              else Nothing)
        f

withFeedGenerator :: FeedSetter (String, Maybe URLString)
withFeedGenerator (gen, mbURI) fe =
  case fe of
    Feed.Types.AtomFeed f ->
      Feed.Types.AtomFeed $
      f
      { Atom.feedGenerator =
        Just
          ((Atom.nullGenerator gen)
           { Atom.genURI = mbURI
           })
      }
    Feed.Types.RSSFeed f ->
      Feed.Types.RSSFeed
        f
        { rssChannel =
          (rssChannel f)
          { rssGenerator = Just gen
          }
        }
    Feed.Types.RSS1Feed f ->
      Feed.Types.RSS1Feed $
      case break isSource $ RSS1.channelDC (RSS1.feedChannel f) of
        (as, dci:bs) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              as ++
              dci
              { dcText = gen
              } :
              bs
            }
          }
        (_, []) ->
          f
          { RSS1.feedChannel =
            (RSS1.feedChannel f)
            { RSS1.channelDC =
              DCItem
              { dcElt = DC_Source
              , dcText = gen
              } :
              RSS1.channelDC (RSS1.feedChannel f)
            }
          }
    Feed.Types.XMLFeed f ->
      Feed.Types.XMLFeed $
      mapMaybeChildren
        (\e ->
            if elName e == unqual "channel"
              then Just
                     (mapMaybeChildren
                        (\e2 ->
                            if elName e2 == unqual "generator"
                              then Just (unode "generator" gen)
                              else Nothing)
                        e)
              else Nothing)
        f
  where
    isSource dc = dcElt dc == DC_Source

-- Item constructors (all the way to the end):
atomEntryToItem :: Atom.Entry -> Feed.Types.Item
atomEntryToItem = Feed.Types.AtomItem

rssItemToItem :: RSS.RSSItem -> Feed.Types.Item
rssItemToItem = Feed.Types.RSSItem

rdfItemToItem :: RSS1.Item -> Feed.Types.Item
rdfItemToItem = Feed.Types.RSS1Item

type ItemSetter a = a -> Feed.Types.Item -> Feed.Types.Item

-- | 'withItemPubDate dt' associates the creation\/ publication date 'dt'
-- with a feed item.
withItemPubDate :: ItemSetter DateString
withItemPubDate dt fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryUpdated = dt
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemPubDate = Just dt
        }
    Feed.Types.RSS1Item i ->
      case break isDate $ RSS1.itemDC i of
        (as, dci:bs) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              as ++
              dci
              { dcText = dt
              } :
              bs
            }
        (_, []) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              DCItem
              { dcElt = DC_Date
              , dcText = dt
              } :
              RSS1.itemDC i
            }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "pubDate" dt) $
      filterChildren (\e -> elName e /= unqual "pubDate") i
  where
    isDate dc = dcElt dc == DC_Date

-- | 'withItemDate' is a synonym for 'withItemPubDate'.
withItemDate :: ItemSetter DateString
withItemDate = withItemPubDate

-- | 'withItemTitle myTitle' associates a new title, 'myTitle',
-- with a feed item.
withItemTitle :: ItemSetter String
withItemTitle tit fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryTitle = TextString tit
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemTitle = Just tit
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemTitle = tit
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "title" tit) $
      filterChildren (\e -> elName e /= unqual "title") i

-- | 'withItemAuthor auStr' associates new author info
-- with a feed item.
withItemAuthor :: ItemSetter String
withItemAuthor au fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryAuthors =
          [ nullPerson
            { personName = au
            , personURI = Just au
            }
          ]
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemAuthor = Just au
        }
    Feed.Types.RSS1Item i ->
      case break isAuthor $ RSS1.itemDC i of
        (as, dci:bs) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              as ++
              dci
              { dcText = au
              } :
              bs
            }
        (_, []) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              DCItem
              { dcElt = DC_Creator
              , dcText = au
              } :
              RSS1.itemDC i
            }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "author" au) $
      filterChildren (\e -> elName e /= unqual "author") i
  where
    isAuthor dc = dcElt dc == DC_Creator

-- | 'withItemFeedLink name myFeed' associates the parent feed URL 'myFeed'
-- with a feed item. It is labelled as 'name'.
withItemFeedLink :: String -> ItemSetter String
withItemFeedLink tit url fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entrySource =
          Just
            Atom.nullSource
            { sourceId = Just url
            , sourceTitle = Just (TextString tit)
            }
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemSource = Just (RSS.nullSource url tit)
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemTitle = tit
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "source" (Attr (unqual "url") url, tit)) $
      filterChildren (\e -> elName e /= unqual "source") i

-- | 'withItemCommentLink url' sets the URL reference to the comment page to 'url'.
withItemCommentLink :: ItemSetter String
withItemCommentLink url fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryLinks =
          ((nullLink url)
           { linkRel = Just (Left "replies")
           }) :
          Atom.entryLinks e
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemComments = Just url
        }
    Feed.Types.RSS1Item i ->
      case break isRel $ RSS1.itemDC i of
        (as, dci:bs) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              as ++
              dci
              { dcText = url
              } :
              bs
            }
        (_, []) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              DCItem
              { dcElt = DC_Relation
              , dcText = url
              } :
              RSS1.itemDC i
            }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "comments" url) $
      filterChildren (\e -> elName e /= unqual "comments") i
  where
    isRel dc = dcElt dc == DC_Relation

-- | 'withItemEnclosure url mbTy len' sets the URL reference to the comment page to 'url'.
withItemEnclosure :: String -> Maybe String -> ItemSetter (Maybe Integer)
withItemEnclosure url ty mb_len fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryLinks =
          ((nullLink url)
           { linkRel = Just (Left "enclosure")
           , linkType = ty
           , linkLength = fmap show mb_len
           }) :
          Atom.entryLinks e
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemEnclosure = Just (nullEnclosure url mb_len (fromMaybe "text/html" ty))
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemContent =
          nullContentInfo
          { contentURI = Just url
          , contentFormat = ty
          } :
          RSS1.itemContent i
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem .
      addChild
        ((unode "enclosure" url)
         { elAttribs = [Attr (unqual "length") "0", Attr (unqual "type") (fromMaybe "text/html" ty)]
         }) $
      filterChildren (\e -> elName e /= unqual "enclosure") i

-- | 'withItemId isURL id' associates new unique identifier with a feed item.
-- If 'isURL' is 'True', then the id is assumed to point to a valid web resource.
withItemId :: Bool -> ItemSetter String
withItemId isURL idS fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryId = idS
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemGuid =
          Just
            (nullGuid idS)
            { rssGuidPermanentURL = Just isURL
            }
        }
    Feed.Types.RSS1Item i ->
      case break isId $ RSS1.itemDC i of
        (as, dci:bs) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              as ++
              dci
              { dcText = idS
              } :
              bs
            }
        (_, []) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              DCItem
              { dcElt = DC_Identifier
              , dcText = idS
              } :
              RSS1.itemDC i
            }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem .
      addChild (unode "guid" (Attr (unqual "isPermaLink") (showBool isURL), idS)) $
      filterChildren (\e -> elName e /= unqual "guid") i
  where
    showBool x = map toLower (show x)
    isId dc = dcElt dc == DC_Identifier

-- | 'withItemDescription desc' associates a new descriptive string (aka summary)
-- with a feed item.
withItemDescription :: ItemSetter String
withItemDescription desc fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entrySummary = Just (TextString desc)
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemDescription = Just desc
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemDesc = Just desc
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "description" desc) $
      filterChildren (\e -> elName e /= unqual "description") i

-- | 'withItemRights rightStr' associates the rights information 'rightStr'
-- with a feed item.
withItemRights :: ItemSetter String
withItemRights desc fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryRights = Just (TextString desc)
        }
    -- Note: per-item copyright information isn't supported by RSS2.0 (and earlier editions),
    -- you can only attach this at the feed/channel level. So, there's not much we can do
    -- except dropping the information on the floor here. (Rolling our own attribute or
    -- extension element is an option, but would prefer if someone else had started that
    -- effort already.
    Feed.Types.RSSItem {} -> fi
    Feed.Types.RSS1Item i ->
      case break ((== DC_Rights) . dcElt) $ RSS1.itemDC i of
        (as, dci:bs) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              as ++
              dci
              { dcText = desc
              } :
              bs
            }
        (_, []) ->
          Feed.Types.RSS1Item
            i
            { RSS1.itemDC =
              DCItem
              { dcElt = DC_Rights
              , dcText = desc
              } :
              RSS1.itemDC i
            }
    -- Since we're so far assuming that a shallow XML rep. of an item
    -- is of RSS2.0 ilk, pinning on the rights info is hard (see above.)
    Feed.Types.XMLItem {} -> fi

-- | 'withItemTitle myLink' associates a new URL, 'myLink',
-- with a feed item.
withItemLink :: ItemSetter URLString
withItemLink url fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryLinks = replaceAlternate url (Atom.entryLinks e)
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemLink = Just url
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemLink = url
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem . addChild (unode "link" url) $
      filterChildren (\e -> elName e /= unqual "link") i
  where
    replaceAlternate _ [] = []
    replaceAlternate x (lr:xs)
      | toStr (Atom.linkRel lr) == "alternate" =
        lr
        { Atom.linkHref = x
        } :
        xs
      | otherwise = lr : replaceAlternate x xs
    toStr Nothing = ""
    toStr (Just (Left x)) = x
    toStr (Just (Right x)) = x

withItemCategories :: ItemSetter [(String, Maybe String)]
withItemCategories cats fi =
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem
        e
        { Atom.entryCategories =
          map
            (\(t, mb) ->
                (Atom.newCategory t)
                { Atom.catScheme = mb
                })
            cats ++
          entryCategories e
        }
    Feed.Types.RSSItem i ->
      Feed.Types.RSSItem
        i
        { RSS.rssItemCategories =
          map
            (\(t, mb) ->
                (RSS.newCategory t)
                { RSS.rssCategoryDomain = mb
                })
            cats ++
          rssItemCategories i
        }
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item
        i
        { RSS1.itemDC =
          map
            (\(t, _) ->
                DCItem
                { dcElt = DC_Subject
                , dcText = t
                })
            cats ++
          RSS1.itemDC i
        }
    Feed.Types.XMLItem i ->
      Feed.Types.XMLItem $
      foldr
        (\(t, mb) acc ->
            addChild
              (unode
                 "category"
                 (maybe (: []) (\v x -> [Attr (unqual "domain") v, x]) mb $ Attr (unqual "term") t))
              acc)
        i
        cats

-- helpers..
filterChildren :: (XML.Element -> Bool) -> XML.Element -> XML.Element
filterChildren pre e =
  case elContent e of
    [] -> e
    cs ->
      e
      { elContent = mapMaybe filterElt cs
      }
  where
    filterElt xe@(XML.Elem el)
      | pre el = Just xe
      | otherwise = Nothing
    filterElt xe = Just xe

addChild :: XML.Element -> XML.Element -> XML.Element
addChild a b =
  b
  { elContent = XML.Elem a : elContent b
  }

mapMaybeChildren :: (XML.Element -> Maybe XML.Element) -> XML.Element -> XML.Element
mapMaybeChildren f e =
  case elContent e of
    [] -> e
    cs ->
      e
      { elContent = map procElt cs
      }
  where
    procElt xe@(XML.Elem el) =
      case f el of
        Nothing -> xe
        Just el1 -> XML.Elem el1
    procElt xe = xe
