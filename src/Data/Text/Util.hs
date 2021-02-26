module Data.Text.Util
  ( readInt
  , renderFeed
  , renderFeedWith
  ) where

import Prelude.Compat

import Data.Text
import Data.Text.Read

import qualified Data.Text.Lazy as TL
import qualified Data.XML.Types as XT -- from xml-types
import qualified Text.XML as XC -- from xml-conduit

readInt :: Text -> Maybe Integer
readInt s =
  case decimal s of
    Right (x, _) -> Just x
    _ -> Nothing

renderFeed :: (a -> XT.Element) -> a -> Maybe TL.Text
renderFeed = renderFeedWith XC.def

renderFeedWith :: XC.RenderSettings -> (a -> XT.Element) -> a -> Maybe TL.Text
renderFeedWith opts cf f =
  let e = cf f
      d = elToDoc e
   in XC.renderText opts <$> d

-- Ancillaries --
elToDoc :: XT.Element -> Maybe XC.Document
elToDoc el =
  let txd = XT.Document (XC.Prologue [] Nothing []) el []
      cxd = XC.fromXMLDocument txd
   in either (const Nothing) Just cxd
