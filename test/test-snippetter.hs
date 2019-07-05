module Main where

import Test.Tasty
import qualified Tests.LayoutBaseBasics
import qualified Tests.Content
import qualified Tests.Actions
import qualified Tests.Utilities

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "LayoutBase basics" Tests.LayoutBaseBasics.tests
    , testGroup "Content" Tests.Content.tests
    , testGroup "Actions" Tests.Actions.tests
    , testGroup "Utilities" Tests.Utilities.tests
    ]

main :: IO ()
main = defaultMain tests
