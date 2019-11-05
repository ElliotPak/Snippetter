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
import Debug.Trace

files =
  [ ("foo", ("- title: file1", startTime))
  , ("bar", ("- title: file2", startTime))
  , ("baz", ("- title: file3", startTime))
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
  , testCase "Params" $
    passPlus' buildMeta2 (loadLayoutFile bmap "test")
      [ bb "test" "title: thing1" "thing1"
      , bb "test" "title: thing2" "thing2"
      ]
  , testCase "Files" $
    passPlus' buildMeta3 (loadLayoutFile bmap "test")
      [ bb "foo" "title: file1" "file1"
      , bb "bar" "title: file2" "file2"
      ]
  , testCase "Params, Files" $
    passPlus' buildMeta4 (loadLayoutFile bmap "test")
      [ bb "test" "title: thing1" "thing1"
      , bb "test" "title: thing2" "thing2"
      , bb "foo" "title: file1" "file1"
      , bb "bar" "title: file2" "file2"
      ]
  , testCase "Default" $
    passPlus' buildMeta5 (loadLayoutFile bmap "test") []
  , testCase "Params, Default" $
    passPlus' buildMeta6 (loadLayoutFile bmap "test")
      [ bb "test" "extra: thing\ntitle: thing1" "thing1"
      , bb "test" "extra: thing\ntitle: thing2" "thing2"
      ]
  , testCase "Files, Default" $
    passPlus' buildMeta7 (loadLayoutFile bmap "test")
      [ bb "foo" "extra: thing\ntitle: file1" "file1"
      , bb "bar" "extra: thing\ntitle: file2" "file2"
      ]
  , testCase "Params, Files, Default" $
    passPlus' buildMeta8 (loadLayoutFile bmap "test")
      [ bb "test" "extra: thing\ntitle: thing1" "thing1"
      , bb "test" "extra: thing\ntitle: thing2" "thing2"
      , bb "foo" "extra: thing\ntitle: file1" "file1"
      , bb "bar" "extra: thing\ntitle: file2" "file2"
      ]
  ]
  where
    pathedSA x = PathedSiteAction x (Just "test")
    pathedP' source x = PathedParams x (Just source)
    bb source params out = pathedSA $ Build basicBuilder (pathedP' source $ paramsFromText params) out
    base = "- type: build-meta\n  builder-name: multi-builder\n  parameters:\n    builder-name: basic\n    filename-key: title\n    "
    bNoParams = "params: []\n    "
    bParams = "params:\n    - title: thing1\n    - title: thing2\n    "
    bNoFiles = "files: []\n    "
    bFiles = "files:\n    - foo\n    - bar\n    "
    bNoDefault = "default: {}"
    bDefault = "default:\n      extra: thing"
    buildMeta1 = base <> bNoParams <> bNoFiles <> bNoDefault
    buildMeta2 = base <> bParams <> bNoFiles <> bNoDefault
    buildMeta3 = base <> bNoParams <> bFiles <> bNoDefault
    buildMeta4 = base <> bParams <> bFiles <> bNoDefault
    buildMeta5 = base <> bNoParams <> bNoFiles <> bDefault
    buildMeta6 = base <> bParams <> bNoFiles <> bDefault
    buildMeta7 = base <> bNoParams <> bFiles <> bDefault
    buildMeta8 = base <> bParams <> bFiles <> bDefault
