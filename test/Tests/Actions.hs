{-# LANGUAGE OverloadedStrings #-}

module Tests.Actions
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Snippetter.Build
import Snippetter.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testGroup "No Content Actions" testNoCon
  , testGroup "Single Content Actions" testSingleCon
  , testGroup "Multi Content Actions" testMultiCon
  , testGroup "No Action" testNoAct
  , testGroup "Built-in Actions" testBuiltIn
  ]

testNoAct =
  [ testCase "File deps" testNoActFiles
  , testCase "Show" testNoActShow
  , testCase "Preview" testNoActPreview
  , testCase "Executing" testNoActExecute
  ]

testNoActFiles = passIO (actNeededFiles noAction) HS.empty

testNoActShow = actShow noAction @?= "No action"

testNoActPreview = passIO (actPreview noAction) "No action"

testNoActExecute = passIO (actExecute noAction "foo") "foo"

testNoCon =
  [ testCase "File deps" testNoConFiles
  , testCase "Show" testNoConShow
  , testCase "Preview" testNoConPreview
  , testCase "Executing" testNoConExecute
  ]

noConAction = noContentAction ((<>) "foo") "Test action"

testNoConFiles = passIO (actNeededFiles noConAction) HS.empty

testNoConShow = actShow noConAction @?= "Test action"

testNoConPreview = passIO (actPreview noConAction) "Test action"

testNoConExecute = passIO (actExecute noConAction "bar") "foobar"

testSingleCon =
  [ testGroup "File deps" testSingleConFiles
  , testCase "Show" testSingleConShow
  , testCase "Preview" testSingleConPreview
  , testCase "Executing" testSingleConExecute
  ]

singleConAction1 = singleContentAction (text "foo") func "Test: "
  where
    func a b = a <> b <> b

singleConAction2 = singleContentAction (snippet "foo") func "Test: "
  where
    func a b = a <> b <> b

testSingleConFiles =
  [ testCase "Content doesn't need files" $
    passIO (actNeededFiles singleConAction1) HS.empty
  , testCase "Content does need files" $
    passIO (actNeededFiles singleConAction2) $ HS.singleton "foo"
  ]

testSingleConShow = actShow singleConAction1 @?= "Test: \"foo\""

testSingleConPreview = passIO (actPreview singleConAction1) "Test: \"foo\""

testSingleConExecute = passIO (actExecute singleConAction1 "bar") "barfoofoo"

testMultiCon =
  [ testGroup "File deps" testMultiConFiles
  , testCase "Show" testMultiConShow
  , testCase "Preview" testMultiConPreview
  , testCase "Executing" testMultiConExecute
  ]

multiConAction1 = multiContentAction [text "foo", text "bar"] func "Test: "
  where
    func a b = a <> head b <> b !! 1

multiConAction2 =
  multiContentAction [snippet "foo", snippet "bar"] func "Test: "
  where
    func a b = a <> head b <> b !! 1

testMultiConFiles =
  [ testCase "Content doesn't need files" $
    passIO (actNeededFiles multiConAction1) HS.empty
  , testCase "Content does need files" $
    passIO (actNeededFiles multiConAction2) $ HS.fromList ["foo", "bar"]
  ]

testMultiConShow =
  actShow multiConAction1 @?= "Test: \n  - \"foo\"\n  - \"bar\""

testMultiConPreview =
  passIO (actPreview multiConAction1) "Test: \n  - \"foo\"\n  - \"bar\""

testMultiConExecute = passIO (actExecute multiConAction1 "baz") "bazfoobar"

testBuiltIn = [testGroup "Add" testAdd, testGroup "Replace" testReplace]

testAdd =
  [testCase "Executing" $ passIO (actExecute (add $ text "foo") "bar") "barfoo"]

testReplace =
  [ testCase "Executing" $
    passIO (actExecute (replace "x" $ text "foo") "xbarxx") "foobarfoofoo"
  ]
