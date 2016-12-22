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
  , parseFeedSource -- :: XmlSource s => s -> Maybe Feed
   -- if you know your format, use these directly:
  , readRSS2 -- :: XML.Element -> Maybe Feed
  , readRSS1 -- :: XML.Element -> Maybe Feed
  , readAtom -- :: XML.Element -> Maybe Feed
  ) where

import Text.Atom.Feed.Import as Atom
import Text.RSS.Import as RSS
import Text.RSS1.Import as RSS1

import Text.Feed.Types
import Text.XML.Light as XML
import Text.XML.Light.Lexer (XmlSource)

import Control.Monad
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
-- | 'parseFeedFromFile fp' reads in the contents of the file at @fp@;
-- the assumed encoding is UTF-8.
parseFeedFromFile :: FilePath -> IO Feed
parseFeedFromFile fp = do
  ls <- utf8readFile fp
  case parseFeedString ls of
    Nothing -> fail ("parseFeedFromFile: not a well-formed XML content in: " ++ fp)
    Just f -> return f

-- | 'parseFeedWithParser tries to parse the string @str@
-- as one of the feed formats. First as Atom, then RSS2 before
-- giving RSS1 a try. @Nothing@ is, rather unhelpfully, returned
-- as an indication of error.
parseFeedWithParser
  :: XmlSource s
  => (s -> Maybe Element) -> s -> Maybe Feed
parseFeedWithParser parser str =
  case parser str of
    Nothing -> Nothing
    Just e -> readAtom e `mplus` readRSS2 e `mplus` readRSS1 e `mplus` Just (XMLFeed e)

parseFeedString :: String -> Maybe Feed
parseFeedString = parseFeedSource

-- | 'parseFeedSource s' tries to parse the source @s@ as
-- one of the feed formats. First as Atom, then RSS2 before giving
-- RSS1 a try. @Nothing@ is, rather unhelpfully, returned as an
-- indication of error.
parseFeedSource
  :: XmlSource s
  => s -> Maybe Feed
parseFeedSource = parseFeedWithParser parseXMLDoc

-- | 'readRSS2 elt' tries to derive an RSS2.x, RSS-0.9x feed document
-- from the XML element @e@.
readRSS2 :: XML.Element -> Maybe Feed
readRSS2 = fmap RSSFeed . RSS.elementToRSS

-- | 'readRSS1 elt' tries to derive an RSS1.0 feed document
-- from the XML element @e@.
readRSS1 :: XML.Element -> Maybe Feed
readRSS1 = fmap RSS1Feed . RSS1.elementToFeed

-- | 'readAtom elt' tries to derive an Atom feed document
-- from the XML element @e@.
readAtom :: XML.Element -> Maybe Feed
readAtom = fmap AtomFeed . Atom.elementFeed
