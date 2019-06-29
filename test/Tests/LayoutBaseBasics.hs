module Tests.LayoutBaseBasics (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Snippetter.LayoutBase

tests =
    [ testGroup "Pathed types" testPathedTypes
    ]

testPathedTypes = 
    [ testCase "addPath correctly adds path" testPathedLayout
    , testGroup "PathedParams" testPathedParams
    ]

testPathedLayout =
    addPath "path" (LayoutCopy "a" "b") @?= PathedLayout (LayoutCopy "a" "b") (Just "path")
testPathedParams = []
