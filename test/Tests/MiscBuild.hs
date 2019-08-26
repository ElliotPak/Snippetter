module Tests.MiscBuild
  ( tests
  ) where

import Snippetter.Layout
import Test.Tasty
import Test.Tasty.HUnit

tests = [testGroup "Pathed types" testPathedTypes]

testPathedTypes =
  [ testCase "addPath correctly adds path" testPathedLayout
  , testGroup "PathedParams" testPathedParams
  ]

testPathedLayout =
  addPath "path" (LayoutCopy "a" "b") @?=
  PathedLayout (LayoutCopy "a" "b") (Just "path")

testPathedParams = []
