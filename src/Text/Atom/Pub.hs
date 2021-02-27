--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Pub
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Types for the Atom Publishing Protocol (APP)
--
--------------------------------------------------------------------
module Text.Atom.Pub
  ( Service(..)
  , Workspace(..)
  , Collection(..)
  , Categories(..)
  , Accept(..)
  ) where

import Prelude.Compat

import Data.Text (Text)
import Data.XML.Types as XML
import Text.Atom.Feed (Category, TextContent, URI)

data Service =
  Service
    { serviceWorkspaces :: [Workspace]
    , serviceOther :: [XML.Element]
    }

data Workspace =
  Workspace
    { workspaceTitle :: TextContent
    , workspaceCols :: [Collection]
    , workspaceOther :: [XML.Element]
    }

data Collection =
  Collection
    { collectionURI :: URI
    , collectionTitle :: TextContent
    , collectionAccept :: [Accept]
    , collectionCats :: [Categories]
    , collectionOther :: [XML.Element]
    }

data Categories
  = CategoriesExternal URI
  | Categories (Maybe Bool) (Maybe URI) [Category]
  deriving (Show)

newtype Accept =
  Accept
    { acceptType :: Text
    }
