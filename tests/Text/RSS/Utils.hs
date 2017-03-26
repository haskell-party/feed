module Text.RSS.Utils where

import Prelude ()
import Prelude.Compat

import Text.XML.Light as XML

createContent :: String -> Content
createContent txt =
  Text
    blank_cdata
    { cdData = txt
    }

createQName :: String -> QName
createQName txt =
  XML.QName
  { qName = txt
  , qURI = Nothing
  , qPrefix = Nothing
  }
