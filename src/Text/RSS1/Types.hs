--------------------------------------------------------------------
-- |
-- Module    : Text.DublinCore.Types
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- Representing the DublinCore metadata elements in Haskell.
-- For information on the Dublin Core Metadata Element Set,
-- see: <http://dublincore.org/>
--
module Text.DublinCore.Types
  ( DCItem(..)
  , DCInfo(..)
  , infoToTag
  , dc_element_names
  ) where

-- | A DCItem pairs a specific element with its (string) value.
data DCItem = DCItem
  { dcElt :: DCInfo
  , dcText :: String
  } deriving (Eq, Show)

-- | The Dublin Core Metadata Element Set, all 15 of them (plus an extension constructor.)
data DCInfo
  = DC_Title -- ^ A name given to the resource.
  | DC_Creator -- ^ An entity primarily responsible for making the content of the resource.
  | DC_Subject -- ^ The topic of the content of the resource.
  | DC_Description -- ^ An account of the content of the resource.
  | DC_Publisher -- ^ An entity responsible for making the resource available
  | DC_Contributor -- ^ An entity responsible for making contributions to the content of the resource.
  | DC_Date -- ^ A date associated with an event in the life cycle of the resource (YYYY-MM-DD)
  | DC_Type -- ^ The nature or genre of the content of the resource.
  | DC_Format -- ^ The physical or digital manifestation of the resource.
  | DC_Identifier -- ^ An unambiguous reference to the resource within a given context.
  | DC_Source -- ^ A Reference to a resource from which the present resource is derived.
  | DC_Language -- ^ A language of the intellectual content of the resource.
  | DC_Relation -- ^ A reference to a related resource.
  | DC_Coverage -- ^ The extent or scope of the content of the resource.
  | DC_Rights -- ^ Information about rights held in and over the resource.
  | DC_Other String -- ^ Other; data type extension mechanism.
  deriving (Eq, Show)

infoToTag :: DCInfo -> String
infoToTag i =
  case i of
    DC_Title -> "title"
    DC_Creator -> "creator"
    DC_Subject -> "subject"
    DC_Description -> "description"
    DC_Publisher -> "publisher"
    DC_Contributor -> "contributor"
    DC_Date -> "date"
    DC_Type -> "type"
    DC_Format -> "format"
    DC_Identifier -> "identifier"
    DC_Source -> "source"
    DC_Language -> "language"
    DC_Relation -> "relation"
    DC_Coverage -> "coverage"
    DC_Rights -> "rights"
    DC_Other o -> o

dc_element_names :: [String]
dc_element_names =
  [ "title"
  , "creator"
  , "subject"
  , "description"
  , "publisher"
  , "contributor"
  , "date"
  , "type"
  , "format"
  , "identifier"
  , "source"
  , "language"
  , "relation"
  , "coverage"
  , "rights"
  ]
