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

import Prelude.Compat

import Data.Text (Text)
import Data.XML.Compat
import Data.XML.Types as XML

-- * Core Types
-- ^The Radio Userland version of RSS documents\/feeds.
-- (versions 0.9x, 2.x)
data RSS =
  RSS
    { rssVersion :: Text
    , rssAttrs :: [Attr]
    , rssChannel :: RSSChannel
    , rssOther :: [XML.Element]
    }
  deriving (Show)

type URLString = Text

-- | RFC 822 conforming.
type DateString = Text

data RSSChannel =
  RSSChannel
    { rssTitle :: Text
    , rssLink :: URLString
    , rssDescription :: Text
    , rssItems :: [RSSItem]
    , rssLanguage :: Maybe Text
    , rssCopyright :: Maybe Text
    , rssEditor :: Maybe Text
    , rssWebMaster :: Maybe Text
    , rssPubDate :: Maybe DateString -- ^ rfc 822 conforming.
    , rssLastUpdate :: Maybe DateString -- ^ rfc 822 conforming.
    , rssCategories :: [RSSCategory]
    , rssGenerator :: Maybe Text
    , rssDocs :: Maybe URLString
    , rssCloud :: Maybe RSSCloud
    , rssTTL :: Maybe Integer
    , rssImage :: Maybe RSSImage
    , rssRating :: Maybe Text
    , rssTextInput :: Maybe RSSTextInput
    , rssSkipHours :: Maybe [Integer]
    , rssSkipDays :: Maybe [Text]
    , rssChannelOther :: [XML.Element]
    }
  deriving (Show)

data RSSItem =
  RSSItem
    { rssItemTitle :: Maybe Text
    , rssItemLink :: Maybe URLString
    , rssItemDescription :: Maybe Text -- ^if not present, the title is. (per spec, at least.)
    , rssItemAuthor :: Maybe Text
    , rssItemCategories :: [RSSCategory]
    , rssItemComments :: Maybe URLString
    , rssItemContent :: Maybe Text
    , rssItemEnclosure :: Maybe RSSEnclosure
    , rssItemGuid :: Maybe RSSGuid
    , rssItemPubDate :: Maybe DateString
    , rssItemSource :: Maybe RSSSource
    , rssItemAttrs :: [Attr]
    , rssItemOther :: [XML.Element]
    }
  deriving (Show)

data RSSSource =
  RSSSource
    { rssSourceURL :: URLString
    , rssSourceAttrs :: [Attr]
    , rssSourceTitle :: Text
    }
  deriving (Show)

data RSSEnclosure =
  RSSEnclosure
    { rssEnclosureURL :: URLString
    , rssEnclosureLength :: Maybe Integer
    , rssEnclosureType :: Text
    , rssEnclosureAttrs :: [Attr]
    }
  deriving (Show)

data RSSCategory =
  RSSCategory
    { rssCategoryDomain :: Maybe Text
    , rssCategoryAttrs :: [Attr]
    , rssCategoryValue :: Text
    }
  deriving (Show)

data RSSGuid =
  RSSGuid
    { rssGuidPermanentURL :: Maybe Bool
    , rssGuidAttrs :: [Attr]
    , rssGuidValue :: Text
    }
  deriving (Show)

data RSSImage =
  RSSImage
    { rssImageURL :: URLString -- the URL to the image resource.
    , rssImageTitle :: Text
    , rssImageLink :: URLString -- URL that the image resource should be an href to.
    , rssImageWidth :: Maybe Integer
    , rssImageHeight :: Maybe Integer
    , rssImageDesc :: Maybe Text
    , rssImageOther :: [XML.Element]
    }
  deriving (Show)

data RSSCloud =
  RSSCloud
    { rssCloudDomain :: Maybe Text
    , rssCloudPort :: Maybe Text -- on purpose (i.e., not an int)
    , rssCloudPath :: Maybe Text
    , rssCloudRegisterProcedure :: Maybe Text
    , rssCloudProtocol :: Maybe Text
    , rssCloudAttrs :: [Attr]
    }
  deriving (Show)

data RSSTextInput =
  RSSTextInput
    { rssTextInputTitle :: Text
    , rssTextInputDesc :: Text
    , rssTextInputName :: Text
    , rssTextInputLink :: URLString
    , rssTextInputAttrs :: [Attr]
    , rssTextInputOther :: [XML.Element]
    }
  deriving (Show)

-- * Default Constructors:
nullRSS ::
     Text -- ^channel title
  -> URLString -- ^channel link
  -> RSS
nullRSS title link =
  RSS {rssVersion = "2.0", rssAttrs = [], rssChannel = nullChannel title link, rssOther = []}

nullChannel ::
     Text -- ^rssTitle
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

nullItem ::
     Text -- ^title
  -> RSSItem
nullItem title =
  RSSItem
    { rssItemTitle = Just title
    , rssItemLink = Nothing
    , rssItemDescription = Nothing
    , rssItemAuthor = Nothing
    , rssItemCategories = []
    , rssItemComments = Nothing
    , rssItemContent = Nothing
    , rssItemEnclosure = Nothing
    , rssItemGuid = Nothing
    , rssItemPubDate = Nothing
    , rssItemSource = Nothing
    , rssItemAttrs = []
    , rssItemOther = []
    }

nullSource ::
     URLString -- ^source URL
  -> Text -- ^title
  -> RSSSource
nullSource url title = RSSSource {rssSourceURL = url, rssSourceAttrs = [], rssSourceTitle = title}

nullEnclosure ::
     URLString -- ^enclosure URL
  -> Maybe Integer -- ^enclosure length
  -> Text -- ^enclosure type
  -> RSSEnclosure
nullEnclosure url mb_len ty =
  RSSEnclosure
    { rssEnclosureURL = url
    , rssEnclosureLength = mb_len
    , rssEnclosureType = ty
    , rssEnclosureAttrs = []
    }

newCategory ::
     Text -- ^category Value
  -> RSSCategory
newCategory nm =
  RSSCategory {rssCategoryDomain = Nothing, rssCategoryAttrs = [], rssCategoryValue = nm}

nullGuid ::
     Text -- ^guid value
  -> RSSGuid
nullGuid v = RSSGuid {rssGuidPermanentURL = Nothing, rssGuidAttrs = [], rssGuidValue = v}

nullPermaGuid ::
     Text -- ^guid value
  -> RSSGuid
nullPermaGuid v = (nullGuid v) {rssGuidPermanentURL = Just True}

nullImage ::
     URLString -- ^imageURL
  -> Text -- ^imageTitle
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

nullTextInput ::
     Text -- ^inputTitle
  -> Text -- ^inputName
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
