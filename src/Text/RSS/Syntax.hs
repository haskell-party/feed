--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Syntax
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- The basic syntax for putting together feeds.
--
-- For instance, to create a feed with a single item item:
--  (nullRSS \"rss title\" \"link\") {rssChannel=(nullChannel \"channel title\" \"link\") {rssItems=[(nullItem \"item title\")]}}
--------------------------------------------------------------------
module Text.RSS.Syntax
  ( RSS(..)
  , URLString
  , DateString
  , RSSChannel(..)
  , RSSItem(..)
  , RSSSource(..)
  , RSSEnclosure(..)
  , RSSCategory(..)
  , RSSGuid(..)
  , RSSImage(..)
  , RSSCloud(..)
  , RSSTextInput(..)
  , nullRSS
  , nullChannel
  , nullItem
  , nullSource
  , nullEnclosure
  , newCategory
  , nullGuid
  , nullPermaGuid
  , nullImage
  , nullCloud
  , nullTextInput
  ) where

import Prelude ()
import Prelude.Compat

import Text.XML.Light as XML

-- * Core Types
-- ^The Radio Userland version of RSS documents\/feeds.
-- (versions 0.9x, 2.x)
data RSS = RSS
  { rssVersion :: String
  , rssAttrs :: [XML.Attr]
  , rssChannel :: RSSChannel
  , rssOther :: [XML.Element]
  } deriving (Show)

type URLString = String

-- | RFC 822 conforming.
type DateString = String

data RSSChannel = RSSChannel
  { rssTitle :: String
  , rssLink :: URLString
  , rssDescription :: String
  , rssItems :: [RSSItem]
  , rssLanguage :: Maybe String
  , rssCopyright :: Maybe String
  , rssEditor :: Maybe String
  , rssWebMaster :: Maybe String
  , rssPubDate :: Maybe DateString -- ^ rfc 822 conforming.
  , rssLastUpdate :: Maybe DateString -- ^ rfc 822 conforming.
  , rssCategories :: [RSSCategory]
  , rssGenerator :: Maybe String
  , rssDocs :: Maybe URLString
  , rssCloud :: Maybe RSSCloud
  , rssTTL :: Maybe Integer
  , rssImage :: Maybe RSSImage
  , rssRating :: Maybe String
  , rssTextInput :: Maybe RSSTextInput
  , rssSkipHours :: Maybe [Integer]
  , rssSkipDays :: Maybe [String]
  , rssChannelOther :: [XML.Element]
  } deriving (Show)

data RSSItem = RSSItem
  { rssItemTitle :: Maybe String
  , rssItemLink :: Maybe URLString
  , rssItemDescription :: Maybe String -- ^if not present, the title is. (per spec, at least.)
  , rssItemAuthor :: Maybe String
  , rssItemCategories :: [RSSCategory]
  , rssItemComments :: Maybe URLString
  , rssItemEnclosure :: Maybe RSSEnclosure
  , rssItemGuid :: Maybe RSSGuid
  , rssItemPubDate :: Maybe DateString
  , rssItemSource :: Maybe RSSSource
  , rssItemAttrs :: [XML.Attr]
  , rssItemOther :: [XML.Element]
  } deriving (Show)

data RSSSource = RSSSource
  { rssSourceURL :: URLString
  , rssSourceAttrs :: [XML.Attr]
  , rssSourceTitle :: String
  } deriving (Show)

data RSSEnclosure = RSSEnclosure
  { rssEnclosureURL :: URLString
  , rssEnclosureLength :: Maybe Integer
  , rssEnclosureType :: String
  , rssEnclosureAttrs :: [XML.Attr]
  } deriving (Show)

data RSSCategory = RSSCategory
  { rssCategoryDomain :: Maybe String
  , rssCategoryAttrs :: [XML.Attr]
  , rssCategoryValue :: String
  } deriving (Show)

data RSSGuid = RSSGuid
  { rssGuidPermanentURL :: Maybe Bool
  , rssGuidAttrs :: [XML.Attr]
  , rssGuidValue :: String
  } deriving (Show)

data RSSImage = RSSImage
  { rssImageURL :: URLString -- the URL to the image resource.
  , rssImageTitle :: String
  , rssImageLink :: URLString -- URL that the image resource should be an href to.
  , rssImageWidth :: Maybe Integer
  , rssImageHeight :: Maybe Integer
  , rssImageDesc :: Maybe String
  , rssImageOther :: [XML.Element]
  } deriving (Show)

data RSSCloud = RSSCloud
  { rssCloudDomain :: Maybe String
  , rssCloudPort :: Maybe String -- on purpose (i.e., not an int)
  , rssCloudPath :: Maybe String
  , rssCloudRegisterProcedure :: Maybe String
  , rssCloudProtocol :: Maybe String
  , rssCloudAttrs :: [XML.Attr]
  } deriving (Show)

data RSSTextInput = RSSTextInput
  { rssTextInputTitle :: String
  , rssTextInputDesc :: String
  , rssTextInputName :: String
  , rssTextInputLink :: URLString
  , rssTextInputAttrs :: [XML.Attr]
  , rssTextInputOther :: [XML.Element]
  } deriving (Show)

-- * Default Constructors:
nullRSS
  :: String -- ^channel title
  -> URLString -- ^channel link
  -> RSS
nullRSS title link =
  RSS
  { rssVersion = "2.0"
  , rssAttrs = []
  , rssChannel = nullChannel title link
  , rssOther = []
  }

nullChannel
  :: String -- ^rssTitle
  -> URLString -- ^rssLink
  -> RSSChannel
nullChannel title link =
  RSSChannel
  { rssTitle = title
  , rssLink = link
  , rssDescription = title
  , rssItems = []
  , rssLanguage = Nothing
  , rssCopyright = Nothing
  , rssEditor = Nothing
  , rssWebMaster = Nothing
  , rssPubDate = Nothing
  , rssLastUpdate = Nothing
  , rssCategories = []
  , rssGenerator = Nothing
  , rssDocs = Nothing
  , rssCloud = Nothing
  , rssTTL = Nothing
  , rssImage = Nothing
  , rssRating = Nothing
  , rssTextInput = Nothing
  , rssSkipHours = Nothing
  , rssSkipDays = Nothing
  , rssChannelOther = []
  }

nullItem
  :: String -- ^title
  -> RSSItem
nullItem title =
  RSSItem
  { rssItemTitle = Just title
  , rssItemLink = Nothing
  , rssItemDescription = Nothing
  , rssItemAuthor = Nothing
  , rssItemCategories = []
  , rssItemComments = Nothing
  , rssItemEnclosure = Nothing
  , rssItemGuid = Nothing
  , rssItemPubDate = Nothing
  , rssItemSource = Nothing
  , rssItemAttrs = []
  , rssItemOther = []
  }

nullSource
  :: URLString -- ^source URL
  -> String -- ^title
  -> RSSSource
nullSource url title =
  RSSSource
  { rssSourceURL = url
  , rssSourceAttrs = []
  , rssSourceTitle = title
  }

nullEnclosure
  :: URLString -- ^enclosure URL
  -> Maybe Integer -- ^enclosure length
  -> String -- ^enclosure type
  -> RSSEnclosure
nullEnclosure url mb_len ty =
  RSSEnclosure
  { rssEnclosureURL = url
  , rssEnclosureLength = mb_len
  , rssEnclosureType = ty
  , rssEnclosureAttrs = []
  }

newCategory
  :: String -- ^category Value
  -> RSSCategory
newCategory nm =
  RSSCategory
  { rssCategoryDomain = Nothing
  , rssCategoryAttrs = []
  , rssCategoryValue = nm
  }

nullGuid
  :: String -- ^guid value
  -> RSSGuid
nullGuid v =
  RSSGuid
  { rssGuidPermanentURL = Nothing
  , rssGuidAttrs = []
  , rssGuidValue = v
  }

nullPermaGuid
  :: String -- ^guid value
  -> RSSGuid
nullPermaGuid v =
  (nullGuid v)
  { rssGuidPermanentURL = Just True
  }

nullImage
  :: URLString -- ^imageURL
  -> String -- ^imageTitle
  -> URLString -- ^imageLink
  -> RSSImage
nullImage url title link =
  RSSImage
  { rssImageURL = url
  , rssImageTitle = title
  , rssImageLink = link
  , rssImageWidth = Nothing
  , rssImageHeight = Nothing
  , rssImageDesc = Nothing
  , rssImageOther = []
  }

nullCloud :: RSSCloud
nullCloud =
  RSSCloud
  { rssCloudDomain = Nothing
  , rssCloudPort = Nothing
  , rssCloudPath = Nothing
  , rssCloudRegisterProcedure = Nothing
  , rssCloudProtocol = Nothing
  , rssCloudAttrs = []
  }

nullTextInput
  :: String -- ^inputTitle
  -> String -- ^inputName
  -> URLString -- ^inputLink
  -> RSSTextInput
nullTextInput title nm link =
  RSSTextInput
  { rssTextInputTitle = title
  , rssTextInputDesc = title
  , rssTextInputName = nm
  , rssTextInputLink = link
  , rssTextInputAttrs = []
  , rssTextInputOther = []
  }
