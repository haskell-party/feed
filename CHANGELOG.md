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
