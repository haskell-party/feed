{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.RSS.Equals where

import Prelude ()
import Prelude.Compat

import Text.XML.Light (Element(..), Content(..), CData(..))
import Text.RSS.Syntax (RSSCloud(..))

instance Eq Element where
  (Element name1 attribs1 content1 line1) == (Element name2 attribs2 content2 line2) =
    (name1 == name2) && (attribs1 == attribs2) && (content1 == content2) && (line1 == line2)

instance Eq Content where
  Text data1 == Text data2 = data1 == data2
  CRef text1 == CRef text2 = text1 == text2
  Elem elem1 == Elem elem2 = elem1 == elem2
  _ == _ = False

instance Eq CData where
  (CData verbatim1 data1 line1) == (CData verbatim2 data2 line2) =
    (verbatim1 == verbatim2) && (data1 == data2) && (line1 == line2)

instance Eq RSSCloud where
  (RSSCloud rssCloudDomain1 rssCloudPort1 rssCloudPath1 rssCloudRegisterProcedure1 rssCloudProtocol1 rssCloudAttrs1) == (RSSCloud rssCloudDomain2 rssCloudPort2 rssCloudPath2 rssCloudRegisterProcedure2 rssCloudProtocol2 rssCloudAttrs2) =
    (rssCloudDomain1 == rssCloudDomain2) &&
    (rssCloudPort1 == rssCloudPort2) &&
    (rssCloudPath1 == rssCloudPath2) &&
    (rssCloudRegisterProcedure1 == rssCloudRegisterProcedure2) &&
    (rssCloudProtocol1 == rssCloudProtocol2) && (rssCloudAttrs1 == rssCloudAttrs2)
