--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Pub.Export
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability:: portable
-- Description: Serializing APP types (as XML.)
--
-- Serializing Atom Publishing Protocol types as XML.
--
--------------------------------------------------------------------
module Text.Atom.Pub.Export
  ( showServiceDoc
  , mkQName
  , mkElem
  , mkLeaf
  , mkAttr
  , xmlns_app
  , appNS
  , xmlService
  , xmlWorkspace
  , xmlCollection
  , xmlCategories
  , xmlAccept
  ) where

import Prelude ()
import Prelude.Compat

import Text.XML.Light
import Text.Atom.Pub
import Text.Atom.Feed.Export
       (mb, xmlCategory, xmlTitle, xmlns_atom)

showServiceDoc :: Service -> String
showServiceDoc s = showElement (xmlService s)

-- ToDo: old crud; inline away.
mkQName :: Maybe String -> String -> QName
mkQName a b =
  blank_name
  { qPrefix = a
  , qName = b
  }

mkElem :: QName -> [Attr] -> [Element] -> Element
mkElem a b c = node a (b :: [Attr], c :: [Element])

mkLeaf :: QName -> [Attr] -> String -> Element
mkLeaf a b c =
  node
    (a :: QName)
    ( b :: [Attr]
    , [ Text
          blank_cdata
          { cdData = c
          }
      ])

mkAttr :: String -> String -> Attr
mkAttr a =
  Attr
    blank_name
    { qName = a
    }

xmlns_app :: Attr
xmlns_app = Attr (mkQName (Just "xmlns") "app") appNS

appNS :: String
appNS = "http://purl.org/atom/app#"

appName :: String -> QName
appName nc =
  (mkQName (Just "app") nc)
  { qURI = Just appNS
  }

xmlService :: Service -> Element
xmlService s =
  mkElem
    (appName "service")
    [xmlns_app, xmlns_atom]
    (map xmlWorkspace (serviceWorkspaces s) ++ serviceOther s)

xmlWorkspace :: Workspace -> Element
xmlWorkspace w =
  mkElem
    (appName "workspace")
    [mkAttr "xml:lang" "en"]
    (concat [[xmlTitle (workspaceTitle w)], map xmlCollection (workspaceCols w), workspaceOther w])

xmlCollection :: Collection -> Element
xmlCollection c =
  mkElem
    (appName "collection")
    [mkAttr "href" (collectionURI c)]
    (concat
       [ [xmlTitle (collectionTitle c)]
       , map xmlAccept (collectionAccept c)
       , map xmlCategories (collectionCats c)
       , collectionOther c
       ])

xmlCategories :: Categories -> Element
xmlCategories (CategoriesExternal u) = mkElem (appName "categories") [mkAttr "href" u] []
xmlCategories (Categories mbFixed mbScheme cs) =
  mkElem
    (appName "categories")
    (mb
       (\f ->
           mkAttr
             "fixed"
             (if f
                then "yes"
                else "no"))
       mbFixed ++
     mb (mkAttr "scheme") mbScheme)
    (map xmlCategory cs)

xmlAccept :: Accept -> Element
xmlAccept a = mkLeaf (appName "accept") [] (acceptType a)
