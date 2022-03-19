#### 1.3.2.1
* text 2.0 support, thanks to Alexander Batischev.
* Moved the repository to https://github.com/haskell-party/

### 1.3.2.0
* Expose RSS/Atom item content through queries by using `getItemContent`.

### 1.3.0.1
* Add a test to check that validation works on a simple entry.
* Change attribute handling when validating so that type attribute is recognised properly on content.

#### 1.3.0.0
* Reverted changes to `EntryContent` that came in 1.2.0.0. Thanks to Tomas Janousek.

#### 1.2.0.1

* Get rid of xmlns:ns and ns:-prefixed attributes that confused some applications, thanks to Tomas Janousek.
* GHC 8.8 support

## 1.2.0.0

Updated `EntryContent`'s `HTMLContent` to wrap an `XML.Element` instead of `Text`. Thanks to Jake Keuhlen.

## 1.1.0.0

* `parseFeedFromFile` now returns `IO (Maybe Feed)` instead of `IO Feed` to distinguish IO exceptions from parse failures. Thanks to Jake Keuhlen.

#### 1.0.1.0

* Support for GHC 8.6.x libraries

* Add `textFeed` and `textRSS` helpers (thanks to Francesco Ariis)

# 1.0.0.0

* Thanks to Dmitry Dzhus feed has been modernized to use the `text`,
  `xml-types` and `xml-conduit` libraries.

### 0.3.12.0

* Adds support for some fallback parsing of atom feeds to XMLFeed (thanks to Joey Hess)

#### 0.3.11.1

* Add missing file to dist of test-suite (thanks to Sergei Trofimovich)

#### 0.3.11.0

* Add `toFeedDateStringUTC` which uses UTCTime rather than ClockTime (thanks to Emanuel Borsboom)
* Now with explicit export lists!

#### 0.3.10.4

* Fix toFeedDateString time format, It used %s (seconds since epoch) instead of %S (seconds of minute), and %m (month) instead of %M (minute) (thanks to Emanuel Borsboom)

#### 0.3.10.3

* RSS Export: avoid <enclosure length="Nothing"> attribute (thanks to Roman Cheplyaka)

#### 0.3.10.2

* The Atom RFC says that when a link element doesn't specify the "rel"
    attribute, i.e. link relation, it should be interpreted as an
    "alternate" relation.  This makes the feed and item query
    functions treat a missing relation as "alternate".

#### 0.3.10.1

* Allow `HUnit 1.3.*`

### 0.3.10.0

* Add `Text.Feed.Import.parseFeedSource :: XmlSource s => s -> Maybe Feed` (thanks to Dmitry Dzhus)

#### 0.3.9.7

* Add missing modules in test-suite

#### 0.3.9.6

* Fixed the "cloud" having attribute "register" instead of "registerProcedure": https://validator.w3.org/feed/docs/rss2.html#ltcloudgtSubelementOfLtchannelgt (thanks to Daniele Francesconi)

#### 0.3.9.5

* Fix typo "skipDayss" -> "skipDays" in `Text.RSS.Export` (thanks to Daniele Francesconi)

#### 0.3.9.4

* Update maintainer information
* Add test suite

#### 0.3.9.3

* Add support for utf8-string >= 1 && < 1.1 and time 1.5.*

#### 0.3.9.1

* add ref to github repo + .cabal tidying.

### 0.3.9

* tidy up compilation with ghc-7.6(.3), bumped version (but no
  functional changes.)

### 0.3.8

* cabal build fixes.

### 0.3.7

* <feed> parsing: made <title> be optional.
* <entry> parsing: try <published> if <updated> is missing.
