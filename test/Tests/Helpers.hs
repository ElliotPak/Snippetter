{-# LANGUAGE OverloadedStrings #-}

module Tests.Helpers
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Snippetter.Build
import Snippetter.Layout
import Snippetter.Utilities
import Snippetter.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import TestAssist

files =
  [ ("foo", ("- title: from-file", startTime))
  , ("bar", ("- title: from-file", startTime))
  , ("baz", ("- title: from-file", startTime))
  ]

passPlus' ::
     (Eq a, Show a, Eq e, Show e)
  => T.Text
  -> Result e MockIO a
  -> a
  -> Assertion
passPlus' = passPlus files

basicBuilder' :: PageBuilder
basicBuilder' params = do
  temp <- lookupText "title" params
  return $ text temp

basicBuilder = NamedPageBuilder "basic" basicBuilder'

bmap =
 insertPageBuilder "basic" basicBuilder' helperBmap

tests =
  [ testGroup "Built-in Actions" testBuiltInActions
  , testGroup "Built-in Builders" testBuiltInBuilders
  ]

testBuiltInActions = [testGroup "Add" testAdd, testGroup "Replace" testReplace]

testAdd =
  [testCase "Executing" $ passIO (actExecute (add $ text "foo") "bar") "barfoo"]

testReplace =
  [ testCase "Executing" $
    passIO (actExecute (replace "x" $ text "foo") "xbarxx") "foobarfoofoo"
  ]

testBuiltInBuilders = [testGroup "Multi-builder" testMultiBuilder]

testMultiBuilder =
  [ testCase "Nothing" $
    passPlus' buildMeta1 (loadLayoutFile bmap "test") []
  , testCase "Just params" $
    passPlus' buildMeta2 (loadLayoutFile bmap "test")
      [ bb "title: thing1" "thing1"
      , bb "title: thing2" "thing2"
      ]
  ]
  where
    pathedSA x = PathedSiteAction x (Just "test")
    pathedP' x = PathedParams x (Just "test")
    bb params out = pathedSA $ Build basicBuilder (pathedP' $ paramsFromText params) out
    base = "- type: build-meta\n  builder-name: multi-builder\n  parameters:\n    builder-name: basic\n    "
    buildMeta1 = base <> "params: []\n    files: []\n    default: {}\n    filename-key: title"
    buildMeta2 = base <> "params:\n    - title: thing1\n    - title: thing2\n    files: []\n    default: {}\n    filename-key: title"
