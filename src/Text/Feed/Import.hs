{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Import
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- Convert from XML to Feeds.
--
--------------------------------------------------------------------
module Text.Feed.Import
  ( parseFeedFromFile -- :: FilePath -> IO Feed
  , parseFeedString -- :: String -> Maybe Feed
  , parseFeedSource -- :: FeedSource s => s -> Maybe Feed
  , FeedSource
          -- if you know your format, use these directly:
  , readRSS2 -- :: XML.Element -> Maybe Feed
  , readRSS1 -- :: XML.Element -> Maybe Feed
  , readAtom -- :: XML.Element -> Maybe Feed
  ) where

import Prelude.Compat

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack)
import Data.XML.Types as XML

import Text.Atom.Feed.Import as Atom

import Text.Feed.Types
import Text.RSS.Import as RSS
import Text.RSS1.Import as RSS1

import Control.Monad.Compat

import qualified Text.XML as C
#if MIN_VERSION_utf8_string(1,0,0)
import Codec.Binary.UTF8.String (decodeString)
import System.IO (IOMode(..), hGetContents, openBinaryFile)

utf8readFile :: FilePath -> IO String
utf8readFile fp = fmap decodeString (hGetContents =<< openBinaryFile fp ReadMode)
#else
import System.IO.UTF8 as UTF8 (readFile)

utf8readFile :: FilePath -> IO String
utf8readFile = UTF8.readFile
#endif
class FeedSource s where
  parseFeedSourceXML :: s -> Either SomeException C.Document

instance FeedSource ByteString where
  parseFeedSourceXML = C.parseLBS C.def

instance FeedSource Text where
  parseFeedSourceXML = C.parseText C.def

instance FeedSource String where
  parseFeedSourceXML = parseFeedSourceXML . pack

-- | 'parseFeedFromFile fp' reads in the contents of the file at @fp@;
-- the assumed encoding is UTF-8.
parseFeedFromFile :: FilePath -> IO (Maybe Feed)
parseFeedFromFile fp = parseFeedString <$> utf8readFile fp

-- | 'parseFeedWithParser tries to parse the string @str@
-- as one of the feed formats. First as Atom, then RSS2 before
-- giving RSS1 a try. @Nothing@ is, rather unhelpfully, returned
-- as an indication of error.
parseFeedWithParser :: FeedSource s => (s -> Either e C.Document) -> s -> Maybe Feed
parseFeedWithParser parser str =
  case parser str of
    Left _ -> Nothing
    Right d -> readAtom e `mplus` readRSS2 e `mplus` readRSS1 e `mplus` Just (XMLFeed e)
      where e = C.toXMLElement $ C.documentRoot d

parseFeedString :: String -> Maybe Feed
parseFeedString = parseFeedSource

-- | 'parseFeedSource s' tries to parse the source @s@ as
-- one of the feed formats. First as Atom, then RSS2 before giving
-- RSS1 a try. @Nothing@ is, rather unhelpfully, returned as an
-- indication of error.
parseFeedSource :: FeedSource s => s -> Maybe Feed
parseFeedSource = parseFeedWithParser parseFeedSourceXML

-- | 'readRSS2 elt' tries to derive an RSS2.x, RSS-0.9x feed document
-- from the XML element @e@.
readRSS2 :: XML.Element -> Maybe Feed
readRSS2 e = RSSFeed <$> RSS.elementToRSS e

-- | 'readRSS1 elt' tries to derive an RSS1.0 feed document
-- from the XML element @e@.
readRSS1 :: XML.Element -> Maybe Feed
readRSS1 e = RSS1Feed <$> RSS1.elementToFeed e

-- | 'readAtom elt' tries to derive an Atom feed document
-- from the XML element @e@.
readAtom :: XML.Element -> Maybe Feed
readAtom e = AtomFeed <$> Atom.elementFeed e
