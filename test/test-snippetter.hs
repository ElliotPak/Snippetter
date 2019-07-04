module Main where

import Test.Tasty
import qualified Tests.LayoutBaseBasics
import qualified Tests.LayoutTypes

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "LayoutBase basics" Tests.LayoutBaseBasics.tests
    , testGroup "Layout Types" Tests.LayoutTypes.tests
    ]

main :: IO ()
main = defaultMain tests
