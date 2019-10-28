{-# LANGUAGE OverloadedStrings #-}

module Tests.Build
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
import Test.Tasty
import Test.Tasty.HUnit
import TestAssist
import qualified Tests.SubBuilder

files = [("foo", ("bar", startTime))]

filesMissing = [("food", ("bar", startTime))]

tests =
  [ testGroup "Text" testText
  , testGroup "Snippet" testSnippet
  , testGroup "Transform" testTransform
  , testGroup "SubMacro" Tests.SubBuilder.tests
  , testGroup "No Content Actions" testNoCon
  , testGroup "Single Content Actions" testSingleCon
  , testGroup "Multi Content Actions" testMultiCon
  , testGroup "No Action" testNoAct
  ]

testText =
  [ testCase "File deps" testTextFiles
  , testCase "Show" testTextShow
  , testCase "Preview" testTextPreview
  , testCase "Resolving" testTextEvaluate
  ]

testTextFiles = passIO (conNeededFiles $ text $ T.pack "test") HS.empty

testTextShow = conShow (text $ T.pack "test") @?= T.pack "\"test\""

testTextPreview = passIO (conPreview $ text $ T.pack "test") (T.pack "\"test\"")

testTextEvaluate = passIO (conEvaluate $ text $ T.pack "test") (T.pack "test")

testSnippet =
  [ testCase "File deps" testSnippetFiles
  , testGroup "Preview" testSnippetPreview
  , testCase "Show" testSnippetShow
  , testGroup "Resolving" testSnippetEvaluate
  ]

testSnippetFiles = passIO (conNeededFiles $ snippet "foo") $ HS.fromList ["foo"]

testSnippetShow = conShow (snippet "foo") @?= T.pack "Snippet named \"foo\""

testSnippetPreview =
  [ testCase "File exists" $
    passMockFiles files (conPreview $ snippet "foo") $
    T.pack "Snippet named \"foo\" with the contents:\n    bar"
  , testCase "File doesn't exist" $
    failMockFiles
      filesMissing
      (conPreview $ snippet "foo")
      (DocFileError $ NotFound "foo")
  ]

testSnippetEvaluate =
  [ testCase "File exists" $
    passMockFiles files (conEvaluate $ snippet "foo") (T.pack "bar")
  , testCase "File doesn't exist" $
    failMockFiles
      filesMissing
      (conEvaluate $ snippet "foo")
      (DocFileError $ NotFound "foo")
  ]

testTransform =
  [ testGroup "File deps" testTransformFiles
  , testGroup "Preview" testTransformPreview
  , testCase "Show" testTransformShow
  , testGroup "Resolving" testTransformEvaluate
  ]

transErrNoFile =
  transform (text $ T.pack "test") $ \text ->
    return $ text <> T.pack " baz"

transErrFile =
  transform (snippet "foo") $ \text -> return $ text <> T.pack " baz"

transErrNoFileFail =
  transform (text $ T.pack "test") $ \text -> Left $ T.pack "idk"

transErrFileFail = transform (snippet "foo") $ \text -> Left $ T.pack "idk"

testTransformFiles =
  [ testCase "Child doesn't depend on anything" $
    passIO (conNeededFiles transErrNoFile) HS.empty
  , testCase "Child depends on a file" $
    passIO (conNeededFiles transErrFile) $ HS.fromList ["foo"]
  , testCase "Child fails and doesn't depend on anything" $
    passIO (conNeededFiles transErrNoFileFail) HS.empty
  , testCase "Child fails and depends on a file" $
    passIO (conNeededFiles transErrFileFail) $ HS.fromList ["foo"]
  ]

testTransformShow =
  conShow transErrNoFile @?= T.pack "Transformation of: \"test\""

testTransformPreview =
  [ testCase "Dry run is the same as normal preview" $
    passIO (conPreview transErrNoFile) (T.pack "Transformation of: \"test\"")
  , testCase "Child is successfully dry ran" $
    passMockFiles files (conPreview transErrFile) $
    T.pack
      "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
  , testCase "Child is unsuccessfully dry ran" $
    failMockFiles
      filesMissing
      (conPreview transErrFile)
      (DocFileError $ NotFound "foo")
  ]

testTransformEvaluate =
  [ testCase "Child successfully evaluated, transform succeeds" $
    passIO (conEvaluate transErrNoFile) (T.pack "test baz")
  , testCase "Child successfully evaluated, transform fails" $
    failIO (conEvaluate transErrNoFileFail) (TransformFailed "idk")
  , testCase "Child unsuccessfully evaluated, transform succeeds" $
    failMockFiles
      filesMissing
      (conEvaluate transErrFile)
      (DocFileError $ NotFound "foo")
  , testCase "Child unsuccessfully evaluated, transform fails" $
    failMockFiles
      filesMissing
      (conEvaluate transErrFileFail)
      (DocFileError $ NotFound "foo")
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
