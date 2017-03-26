--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Atom.Feed
  ( URI
  , NCName
  , Date
  , MediaType
  , Feed(..)
  , Entry(..)
  , EntryContent(..)
  , Category(..)
  , Generator(..)
  , Link(..)
  , TextContent(..)
  , txtToString
  , Source(..)
  , Person(..)
  , InReplyTo(..)
  , InReplyTotal(..)
  , newCategory
  , nullFeed
  , nullEntry
  , nullGenerator
  , nullLink
  , nullSource
  , nullPerson
  ) where

import Prelude ()
import Prelude.Compat

import qualified Text.XML.Light as XML

-- *Core types
-- NOTE: In the future we may want to have more structured
-- types for these.
type URI = String

type NCName = String

type Date = String

type MediaType = String

data Feed = Feed
  { feedId :: String
  , feedTitle :: TextContent
  , feedUpdated :: Date
  , feedAuthors :: [Person]
  , feedCategories :: [Category]
  , feedContributors :: [Person]
  , feedGenerator :: Maybe Generator
  , feedIcon :: Maybe URI
  , feedLinks :: [Link]
  , feedLogo :: Maybe URI
  , feedRights :: Maybe TextContent
  , feedSubtitle :: Maybe TextContent
  , feedEntries :: [Entry]
  , feedAttrs :: [XML.Attr]
  , feedOther :: [XML.Element]
  } deriving (Show)

data Entry = Entry
  { entryId :: String
  , entryTitle :: TextContent
  , entryUpdated :: Date
  , entryAuthors :: [Person]
  , entryCategories :: [Category]
  , entryContent :: Maybe EntryContent
  , entryContributor :: [Person]
  , entryLinks :: [Link]
  , entryPublished :: Maybe Date
  , entryRights :: Maybe TextContent
  , entrySource :: Maybe Source
  , entrySummary :: Maybe TextContent
  , entryInReplyTo :: Maybe InReplyTo
  , entryInReplyTotal :: Maybe InReplyTotal
  , entryAttrs :: [XML.Attr]
  , entryOther :: [XML.Element]
  } deriving (Show)

data EntryContent
  = TextContent String
  | HTMLContent String
  | XHTMLContent XML.Element
  | MixedContent (Maybe String)
                 [XML.Content]
  | ExternalContent (Maybe MediaType)
                    URI
  deriving (Show)

data Category = Category
  { catTerm :: String -- ^ the tag\/term of the category.
  , catScheme :: Maybe URI -- ^ optional URL for identifying the categorization scheme.
  , catLabel :: Maybe String -- ^ human-readable label of the category
  , catOther :: [XML.Element] -- ^ unknown elements, for extensibility.
  } deriving (Show)

data Generator = Generator
  { genURI :: Maybe URI
  , genVersion :: Maybe String
  , genText :: String
  } deriving (Eq, Show)

data Link = Link
  { linkHref :: URI
    -- ToDo: make the switch over to using the Atom.Feed.Link relation type.
  , linkRel :: Maybe (Either NCName URI)
  , linkType :: Maybe MediaType
  , linkHrefLang :: Maybe String
  , linkTitle :: Maybe String
  , linkLength :: Maybe String
  , linkAttrs :: [XML.Attr]
  , linkOther :: [XML.Element]
  } deriving (Show)

data TextContent
  = TextString String
  | HTMLString String
  | XHTMLString XML.Element
  deriving (Show)

txtToString :: TextContent -> String
txtToString (TextString s) = s
txtToString (HTMLString s) = s
txtToString (XHTMLString x) = show x

data Source = Source
  { sourceAuthors :: [Person]
  , sourceCategories :: [Category]
  , sourceGenerator :: Maybe Generator
  , sourceIcon :: Maybe URI
  , sourceId :: Maybe String
  , sourceLinks :: [Link]
  , sourceLogo :: Maybe URI
  , sourceRights :: Maybe TextContent
  , sourceSubtitle :: Maybe TextContent
  , sourceTitle :: Maybe TextContent
  , sourceUpdated :: Maybe Date
  , sourceOther :: [XML.Element]
  } deriving (Show)

data Person = Person
  { personName :: String
  , personURI :: Maybe URI
  , personEmail :: Maybe String
  , personOther :: [XML.Element]
  } deriving (Show)

data InReplyTo = InReplyTo
  { replyToRef :: URI
  , replyToHRef :: Maybe URI
  , replyToType :: Maybe MediaType
  , replyToSource :: Maybe URI
  , replyToOther :: [XML.Attr]
  , replyToContent :: [XML.Content]
  } deriving (Show)

data InReplyTotal = InReplyTotal
  { replyToTotal :: Integer -- non-negative :)
  , replyToTotalOther :: [XML.Attr]
  } deriving (Show)

-- *Smart Constructors
newCategory
  :: String -- ^catTerm
  -> Category
newCategory t =
  Category
  { catTerm = t
  , catScheme = Nothing
  , catLabel = Just t
  , catOther = []
  }

nullFeed
  :: String -- ^feedId
  -> TextContent -- ^feedTitle
  -> Date -- ^feedUpdated
  -> Feed
nullFeed i t u =
  Feed
  { feedId = i
  , feedTitle = t
  , feedUpdated = u
  , feedAuthors = []
  , feedCategories = []
  , feedContributors = []
  , feedGenerator = Nothing
  , feedIcon = Nothing
  , feedLinks = []
  , feedLogo = Nothing
  , feedRights = Nothing
  , feedSubtitle = Nothing
  , feedEntries = []
  , feedAttrs = []
  , feedOther = []
  }

nullEntry
  :: String -- ^entryId
  -> TextContent -- ^entryTitle
  -> Date -- ^entryUpdated
  -> Entry
nullEntry i t u =
  Entry
  { entryId = i
  , entryTitle = t
  , entryUpdated = u
  , entryAuthors = []
  , entryCategories = []
  , entryContent = Nothing
  , entryContributor = []
  , entryLinks = []
  , entryPublished = Nothing
  , entryRights = Nothing
  , entrySource = Nothing
  , entrySummary = Nothing
  , entryInReplyTo = Nothing
  , entryInReplyTotal = Nothing
  , entryAttrs = []
  , entryOther = []
  }

nullGenerator
  :: String -- ^genText
  -> Generator
nullGenerator t =
  Generator
  { genURI = Nothing
  , genVersion = Nothing
  , genText = t
  }

nullLink
  :: URI -- ^linkHref
  -> Link
nullLink uri =
  Link
  { linkHref = uri
  , linkRel = Nothing
  , linkType = Nothing
  , linkHrefLang = Nothing
  , linkTitle = Nothing
  , linkLength = Nothing
  , linkAttrs = []
  , linkOther = []
  }

nullSource :: Source
nullSource =
  Source
  { sourceAuthors = []
  , sourceCategories = []
  , sourceGenerator = Nothing
  , sourceIcon = Nothing
  , sourceId = Nothing
  , sourceLinks = []
  , sourceLogo = Nothing
  , sourceRights = Nothing
  , sourceSubtitle = Nothing
  , sourceTitle = Nothing
  , sourceUpdated = Nothing
  , sourceOther = []
  }

nullPerson :: Person
nullPerson =
  Person
  { personName = ""
  , personURI = Nothing
  , personEmail = Nothing
  , personOther = []
  }
