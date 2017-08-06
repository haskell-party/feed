# Feed

[![feed](https://img.shields.io/hackage/v/feed.svg)](http://hackage.haskell.org/package/feed)
[![Build Status](https://travis-ci.org/bergmark/feed.svg?branch=master)](https://travis-ci.org/bergmark/feed)

## Goal

Interfacing with *RSS* (v 0.9x, 2.x, 1.0) + *Atom* feeds.

- Parsers
- Pretty Printers
- Querying

To help working with the multiple feed formats we've ended up with
this set of modules providing parsers, pretty printers and some utility
code for querying and just generally working with a concrete
representation of feeds in Haskell.

For basic reading and editing of feeds, consult the documentation of
the Text.Feed.* hierarchy.

## Usage

Building an Atom feed is similar to building an RSS feed, but we'll
arbitrarily pick Atom here:

We'd like to generate the XML for a minimal working example.
Constructing our base `Feed` can use the smart constructor called `nullFeed`:

*This is a pattern the library maintains for smart constructors. If you want the
minimum viable 'X', use the 'nullX' constructor.*


```haskell
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import qualified Text.XML.Light.Output as XML

myFeed :: Atom.Feed
myFeed = Atom.nullFeed
    "http://example.com/atom.xml"       -- ^feedId
    (Atom.TextString "Example Website") -- ^feedTitle
    "2017-08-01"                        -- ^feedUpdated
```

```
> XML.ppElement $ Export.xmlFeed myFeed
"<feed xmlns="http://www.w3.org/2005/Atom">
  <title type="text">Example Website</title>
  <id>http://example.com/atom.xml</id>
  <updated>2017-8-1</updated>
</feed>"
```

The `TextContent` sum type allows us to specify which type of text we're providing.

```haskell
data TextContent
  = TextString String
  | HTMLString String
  | XHTMLString XML.Element
  deriving (Show)
```

A feed isn't very useful without some content though, so we'll need to build up an `Entry`.

```haskell
data Post
  = Post
  { _postedOn :: UTCTime
  , _url :: String
  , _content :: String
  }

examplePosts :: [Post]
```

Our `Post` data type will need to be converted into an `Entry` in order to use it in the top level `Feed`. The required fields for an entry are a url "id" from which the entry can be presence validated, a title for the entry, and a posting date. In this example we'll also add authors, link, and the actual entries content, since we have all of this available in the `Post` provided.

```haskell
toEntry :: Post -> Atom.Entry
toEntry (Post date url content) =
  (Atom.nullEntry
      url -- The ID field. Must be a link to validate.
      (Atom.TextString (take 20 content)) -- Title
      "2017-08-01"
  { Atom.entryAuthors = authors
  , Atom.entryLinks = [Atom.nullLink url]
  , Atom.entryContent = Just (Atom.HTMLContent content)
  }
  where
    authors = [
      Atom.nullPerson { Atom.personName = "J. Smith" } ]
```

From the base feed we created earlier, we can add further details (`Link` and `Entry` content) as well as map our `toEntry` function over the posts we'd like to include in the feed.

```haskell
feed :: [Post] -> Atom.Feed
feed posts =
  myFeed { Atom.feedEntries = fmap toEntry posts
         , Atom.feedLinks = [Atom.nullLink "http://example.com/"]
         }
```

```
> XML.ppElement $ Export.xmlFeed $ feed examplePosts
"<feed xmlns="http://www.w3.org/2005/Atom">
  <title type="text">Example Website</title>
  <id>http://example.com/atom.xml</id>
  <updated>2017-08-01</updated>
  <link href="http://example.com/" />
  <entry>
    <id>http://example.com/2</id>
    <title type="text">Bar. Bar. Bar. Bar. </title>
    <updated>2000-02-02T18:30:00Z</updated>
    <author>
      <name>J. Smith</name>
    </author>
    <content type="html">Bar. Bar. Bar. Bar. Bar. Bar. Bar. Bar. Bar. Bar.</content>
    <link href="http://example.com/2" />
  </entry>
  ...
</feed>"

```
See [here](https://github.com/bergmark/feed/blob/master/tests/Example/CreateAtom.hs) for this content as an uninterrupted running example.
