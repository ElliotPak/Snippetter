{-# LANGUAGE OverloadedStrings #-}

module Tests.Layout
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Snippetter.Build
import Snippetter.Helpers
import Snippetter.IO
import Snippetter.Layout
import Snippetter.Utilities
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

sameFileBuilder :: PageBuilder
sameFileBuilder params = return $ snippet "foo"

noFileBuilder :: PageBuilder
noFileBuilder params = return $ text "foo"

sameFileBuild' = NamedPageBuilder "same" sameFileBuilder 

noFileBuild' = NamedPageBuilder "same" noFileBuilder 

sameFileBuild =
  Build sameFileBuild' (pathedP emptyParams) "output"

noFileBuild =
  Build noFileBuild' (pathedP emptyParams) "output"

tests = 
  [ testGroup "Layout File Parsing" testLayoutParse
  , testGroup "Functions on Actions" testActionTypes
  ]

testActionTypes =
  [ testGroup "Build" testBuild
  , testGroup "Copy" testCopy
  , testGroup "Move" testMove
  , testGroup "Delete" testDelete
  , testGroup "Run" testRun
  ]

testBuild = [testGroup "File deps" testBuildFiles]

testCopy = [testCase "File deps" testCopyFiles]

testMove = [testCase "File deps" testMoveFiles]

testDelete = [testCase "File deps" testDeleteFiles]

testRun = [testCase "File deps" testRunFiles]

testBuildFiles =
  [ testCase "No file deps" $ passIO (saNeededFiles noFileBuild) HS.empty
  , testCase "One file dep" $
    passIO (saNeededFiles sameFileBuild) (HS.singleton "foo")
  ]

testCopyFiles = passIO (saNeededFiles (Copy "bar" "foo")) (HS.singleton "bar")

testMoveFiles = passIO (saNeededFiles (Move "bar" "foo")) (HS.singleton "bar")

testDeleteFiles = passIO (saNeededFiles (Delete "bar")) (HS.singleton "bar")

testRunFiles = passIO (saNeededFiles (Run ["bar"] "")) HS.empty

meta :: MonadReadWorld m => MetaBuilder m
meta _ _ _ = return [Copy "foo" "bar", Move "thing" "thing2"]

bmap = insertMetaBuilder "metatest" meta $ 
    insertPageBuilders [ ("same", sameFileBuilder)
                       , ("no", noFileBuilder)
                       ] helperBmap

testLayoutParse =
  [ testCase "Copy, good example" $
    passPlus' copyGood (loadLayoutFile bmap "test") [pathedSA $ Copy "foo" "bar"]
  , testCase "Move, good example" $
    passPlus' moveGood (loadLayoutFile bmap "test") [pathedSA $ Move "foo" "bar"]
  , testCase "Page builder, good example" $
    passPlus' buildPage1 (loadLayoutFile bmap "test") [pathedSA $ Build sameFileBuild' (pathedP' emptyParams) "outtest"]
  , testCase "Meta builder, good example" $
    passPlus' buildMeta1 (loadLayoutFile bmap "test") [pathedSA $ Copy "foo" "bar", pathedSA $ Move "thing" "thing2"]
  ]
  where
    pathedSA x = PathedSiteAction x (Just "test")
    pathedP' x = PathedParams x (Just "test")
    copyGood = "- type: copy\n  from: foo\n  to: bar"
    moveGood = "- type: move\n  from: foo\n  to: bar"
    buildPage1 = "- type: build-page\n  builder-name: same\n  output: outtest\n  parameters: {}"
    buildMeta1 = "- type: build-meta\n  builder-name: metatest\n  parameters: {}"
