module Main where

import Test.Tasty
import qualified Tests.LayoutBaseBasics
import qualified Tests.Content
import qualified Tests.Actions

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "LayoutBase basics" Tests.LayoutBaseBasics.tests
    , testGroup "Content" Tests.Content.tests
    , testGroup "Actions" Tests.Actions.tests
    ]

main :: IO ()
main = defaultMain tests
