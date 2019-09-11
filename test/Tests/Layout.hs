{-# LANGUAGE OverloadedStrings #-}

module Tests.Layout
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Snippetter.Build
import Snippetter.Helpers
import Snippetter.IO
import Snippetter.Layout
import Snippetter.Utilities
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

files =
  [ ("foo", "- title: from-file")
  , ("bar", "- title: from-file")
  , ("baz", "- title: from-file")
  ]

sameFileBuilder :: Builder
sameFileBuilder params = return $ snippet "foo"

noFileBuilder :: Builder
noFileBuilder params = return $ text "foo"

sameFileBuild =
  Build (UnnamedBuilder sameFileBuilder) (makePathed emptyParams) "output"

noFileBuild =
  Build (UnnamedBuilder noFileBuilder) (makePathed emptyParams) "output"

fromText str =
  unObject (Y.decodeEither' (B.pack str) :: Either Y.ParseException Y.Value)
  where
    unObject (Right (Y.Object o)) = o

makePathed o = PathedParams o Nothing

tests =
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

testRunFiles = passIO (saNeededFiles (Run "bar" [] "")) HS.empty
