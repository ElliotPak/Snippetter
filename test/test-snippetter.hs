module Main where

import Test.Tasty
import qualified Tests.Actions
import qualified Tests.Content
import qualified Tests.FileGraph
import qualified Tests.Layout
import qualified Tests.MiscBuild
import qualified Tests.Utilities

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup "Utilities" Tests.Utilities.tests
    , testGroup "Content" Tests.Content.tests
    , testGroup "Actions" Tests.Actions.tests
    , testGroup "Layout" Tests.Layout.tests
    , testGroup "File Graph" Tests.FileGraph.tests
    , testGroup "Misc. Build stuff" Tests.MiscBuild.tests
    ]

main :: IO ()
main = defaultMain tests
