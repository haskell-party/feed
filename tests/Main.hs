module Main (main) where

import Test.Framework (defaultMain)
import Text.RSS.Tests (rssTests)
import Text.Atom.Tests (atomTests)

main :: IO ()
main = defaultMain
  [ rssTests
  , atomTests
  ]
