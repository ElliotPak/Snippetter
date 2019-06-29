module Main where

import Test.Tasty
import qualified Tests.LayoutBaseBasics

tests :: TestTree
tests = testGroup "tests"
    [ testGroup "LayoutBase basics" Tests.LayoutBaseBasics.tests
    ]

main :: IO ()
main = defaultMain tests
