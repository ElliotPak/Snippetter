module Main where

import Test.Tasty
import qualified Tests.Build
import qualified Tests.FileGraph
import qualified Tests.Layout
import qualified Tests.Utilities
import qualified Tests.Helpers

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup "Utilities" Tests.Utilities.tests
    , testGroup "Build" Tests.Build.tests
    , testGroup "Layout" Tests.Layout.tests
    , testGroup "File Graph" Tests.FileGraph.tests
    , testGroup "Helpers" Tests.Helpers.tests
    ]

main :: IO ()
main = defaultMain tests
