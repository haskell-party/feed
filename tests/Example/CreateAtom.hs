module Example.CreateAtom
  ( createAtom
  ) where

import Prelude ()
import Prelude.Compat

import Data.List (intercalate)
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import qualified Text.XML.Light.Output as XML
import Data.Time.Format (parseTimeOrError, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)

createAtom :: String
createAtom = feed examplePosts

data Post
  = Post
  { _postedOn :: UTCTime
  , _url :: String
  , _content :: String
  }

examplePosts :: [Post]
examplePosts =
  [ Post (toTimestamp "2000-02-02T18:30:00Z") "http://example.com/2" $ repeatJoin 10 "Bar."
  , Post (toTimestamp "2000-01-01T18:30:00Z") "http://example.com/1" $ repeatJoin 10 "Foo."
  ]
  where
    toTimestamp = parseTimeOrError True defaultTimeLocale "%FT%T%QZ"
    repeatJoin n = intercalate " " . replicate n

feed :: [Post] -> String
feed posts =
  XML.ppElement . Export.xmlFeed $
    myFeed { Atom.feedEntries = fmap toEntry posts
           , Atom.feedLinks = [Atom.nullLink "http://example.com/"]
           }
  where
    myFeed :: Atom.Feed
    myFeed = Atom.nullFeed
        "http://example.com/atom.xml"
        (Atom.TextString "Example Website") -- Title
        (fromMaybe "" maybeLatestDate)

    maybeLatestDate :: Maybe String
    maybeLatestDate = formatUTC <$> (_postedOn <$> headMaybe posts)

toEntry :: Post -> Atom.Entry
toEntry (Post date url content) =
  (Atom.nullEntry
      url -- The ID field. Must be a link to validate.
      (Atom.TextString (take 20 content)) -- Title
      (formatUTC date))
  { Atom.entryAuthors = authors
  , Atom.entryLinks = [Atom.nullLink url]
  , Atom.entryContent = Just (Atom.HTMLContent content)
  }
  where
    authors = [
      Atom.nullPerson { Atom.personName = "J. Smith" } ]

headMaybe :: [a] -> Maybe a
headMaybe (x:_) =Just x
headMaybe _ = Nothing

formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%FT%T%QZ"
