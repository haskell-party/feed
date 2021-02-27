--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Syntax
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.RSS1.Syntax
  ( URIString
  , TitleString
  , TimeString
  , TextString
  , Feed(..)
  , Channel(..)
  , Image(..)
  , Item(..)
  , TextInputInfo(..)
  , TaxonomyTopic(..)
  , UpdatePeriod(..)
  , ContentInfo(..)
  , nullFeed
  , nullChannel
  , nullImage
  , nullItem
  , nullTextInputInfo
  , nullTaxonomyTopic
  , nullContentInfo
  ) where

import Prelude.Compat

import Data.Text
import Data.XML.Compat
import Data.XML.Types as XML
import Text.DublinCore.Types

type URIString = Text

type TitleString = Text

type TimeString = Text

type TextString = Text

data Feed =
  Feed
    { feedVersion :: Text
    , feedChannel :: Channel
    , feedImage :: Maybe Image
    , feedItems :: [Item]
    , feedTextInput :: Maybe TextInputInfo
    , feedTopics :: [TaxonomyTopic]
    , feedOther :: [XML.Element]
    , feedAttrs :: [Attr]
    }
  deriving (Show)

data Channel =
  Channel
    { channelURI :: URIString
    , channelTitle :: TitleString
    , channelLink :: URIString
    , channelDesc :: TextString
           -- these are indirect RDF associations to elements declared
           -- outside the channel element in the RDF \/ feed document.
    , channelImageURI :: Maybe URIString
    , channelItemURIs :: [URIString]
    , channelTextInputURI :: Maybe URIString
    , channelDC :: [DCItem]
    , channelUpdatePeriod :: Maybe UpdatePeriod
    , channelUpdateFreq :: Maybe Integer
    , channelUpdateBase :: Maybe TimeString -- format is yyyy-mm-ddThh:mm
    , channelContent :: [ContentInfo]
    , channelTopics :: [URIString]
    , channelOther :: [XML.Element]
    , channelAttrs :: [Attr]
    }
  deriving (Show)

data Image =
  Image
    { imageURI :: URIString -- the image resource, most likely.
    , imageTitle :: TextString -- the "alt"ernative text.
    , imageURL :: URIString
    , imageLink :: URIString -- the href of the rendered img resource.
    , imageDC :: [DCItem]
    , imageOther :: [XML.Element]
    , imageAttrs :: [Attr]
    }
  deriving (Show)

data Item =
  Item
    { itemURI :: URIString
    , itemTitle :: TextString
    , itemLink :: URIString
    , itemDesc :: Maybe TextString
    , itemDC :: [DCItem]
    , itemTopics :: [URIString]
    , itemContent :: [ContentInfo]
    , itemOther :: [XML.Element]
    , itemAttrs :: [Attr]
    }
  deriving (Show)

data TextInputInfo =
  TextInputInfo
    { textInputURI :: URIString
    , textInputTitle :: TextString
    , textInputDesc :: TextString
    , textInputName :: TextString
    , textInputLink :: URIString
    , textInputDC :: [DCItem]
    , textInputOther :: [XML.Element]
    , textInputAttrs :: [Attr]
    }
  deriving (Show)

data TaxonomyTopic =
  TaxonomyTopic
    { taxonomyURI :: URIString
    , taxonomyLink :: URIString
    , taxonomyTitle :: Maybe Text
    , taxonomyDesc :: Maybe Text
    , taxonomyTopics :: [URIString]
    , taxonomyDC :: [DCItem]
    , taxonomyOther :: [XML.Element]
    }
  deriving (Show)

data UpdatePeriod
  = Update_Hourly
  | Update_Daily
  | Update_Weekly
  | Update_Monthly
  | Update_Yearly
  deriving (Eq, Show)

data ContentInfo =
  ContentInfo
    { contentURI :: Maybe URIString
    , contentFormat :: Maybe URIString
    , contentEncoding :: Maybe URIString
    , contentValue :: Maybe Text -- should be: RDFValue
    }
  deriving (Eq, Show)

--default constructors:
nullFeed :: URIString -> TitleString -> Feed
nullFeed uri title =
  Feed
    { feedVersion = "1.0"
    , feedChannel = nullChannel uri title
    , feedImage = Nothing
    , feedItems = []
    , feedTextInput = Nothing
    , feedTopics = []
    , feedOther = []
    , feedAttrs = []
    }

nullChannel :: URIString -> TitleString -> Channel
nullChannel uri title =
  Channel
    { channelURI = uri
    , channelTitle = title
    , channelLink = uri
    , channelDesc = title
    , channelImageURI = Nothing
    , channelItemURIs = []
    , channelTextInputURI = Nothing
    , channelDC = []
    , channelUpdatePeriod = Nothing
    , channelUpdateFreq = Nothing
    , channelUpdateBase = Nothing
    , channelContent = []
    , channelTopics = []
    , channelOther = []
    , channelAttrs = []
    }

nullImage :: URIString -> Text -> URIString -> Image
nullImage imguri title link =
  Image
    { imageURI = imguri
    , imageTitle = title
    , imageURL = imguri
    , imageLink = link
    , imageDC = []
    , imageOther = []
    , imageAttrs = []
    }

nullItem :: URIString -> TextString -> URIString -> Item
nullItem uri title link =
  Item
    { itemURI = uri
    , itemTitle = title
    , itemLink = link
    , itemDesc = Nothing
    , itemDC = []
    , itemTopics = []
    , itemContent = []
    , itemOther = []
    , itemAttrs = []
    }

nullTextInputInfo :: URIString -> TextString -> TextString -> URIString -> TextInputInfo
nullTextInputInfo uri title nm link =
  TextInputInfo
    { textInputURI = uri
    , textInputTitle = title
    , textInputDesc = title
    , textInputName = nm
    , textInputLink = link
    , textInputDC = []
    , textInputOther = []
    , textInputAttrs = []
    }

nullTaxonomyTopic :: URIString -> URIString -> TaxonomyTopic
nullTaxonomyTopic uri link =
  TaxonomyTopic
    { taxonomyURI = uri
    , taxonomyLink = link
    , taxonomyTitle = Nothing
    , taxonomyDesc = Nothing
    , taxonomyTopics = []
    , taxonomyDC = []
    , taxonomyOther = []
    }

nullContentInfo :: ContentInfo
nullContentInfo =
  ContentInfo
    { contentURI = Nothing
    , contentFormat = Nothing
    , contentEncoding = Nothing
    , contentValue = Nothing
    }
