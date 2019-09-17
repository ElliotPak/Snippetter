{-# LANGUAGE OverloadedStrings #-}

module Tests.Content
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
import Tests.Helpers
import qualified Tests.SubBuilder

files = [("foo", ("bar", startTime))]

filesMissing = [("food", ("bar", startTime))]

tests =
  [ testGroup "Text" testText
  , testGroup "Snippet" testSnippet
  , testGroup "Transform" testTransform
  , testGroup "TransformError" testTransformError
  , testGroup "SubMacro" Tests.SubBuilder.tests
  ]

testText =
  [ testCase "File deps" testTextFiles
  , testCase "Show" testTextShow
  , testCase "Preview" testTextPreview
  , testCase "Resolving" testTextEvaluate
  ]

testTextFiles = passIO (conNeededFiles $ text $ T.pack "test") HS.empty

testTextShow = conShow (text $ T.pack "test") @?= (T.pack "\"test\"")

testTextPreview = passIO (conPreview $ text $ T.pack "test") (T.pack "\"test\"")

testTextEvaluate = passIO (conEvaluate $ text $ T.pack "test") (T.pack "test")

testSnippet =
  [ testCase "File deps" testSnippetFiles
  , testGroup "Preview" testSnippetPreview
  , testCase "Show" testSnippetShow
  , testGroup "Resolving" testSnippetEvaluate
  ]

testSnippetFiles = passIO (conNeededFiles $ Snippet "foo") $ HS.fromList ["foo"]

testSnippetShow = conShow (snippet "foo") @?= (T.pack "Snippet named \"foo\"")

testSnippetPreview =
  [ testCase "File exists" $
    passMockFiles files (conPreview $ Snippet "foo") $
    T.pack "Snippet named \"foo\" with the contents:\n    bar"
  , testCase "File doesn't exist" $
    failMockFiles
      filesMissing
      (conPreview $ Snippet "foo")
      (DocFileError $ NotFound "foo")
  ]

testSnippetEvaluate =
  [ testCase "File exists" $
    passMockFiles files (conEvaluate $ Snippet "foo") (T.pack "bar")
  , testCase "File doesn't exist" $
    failMockFiles
      filesMissing
      (conEvaluate $ Snippet "foo")
      (DocFileError $ NotFound "foo")
  ]

testTransform =
  [ testGroup "File deps" testTransformFiles
  , testGroup "Preview" testTransformPreview
  , testCase "Show" testTransformShow
  , testGroup "Resolving" testTransformEvaluate
  ]

transNoFile =
  transform (text $ T.pack "test") $ \text -> text <> (T.pack " baz")

transFile = transform (snippet "foo") $ \text -> text <> (T.pack " baz")

testTransformFiles =
  [ testCase "Child doesn't depend on anything" $
    passIO (conNeededFiles transNoFile) HS.empty
  , testCase "Child depends on a file" $
    passIO (conNeededFiles transFile) $ HS.fromList ["foo"]
  ]

testTransformShow =
  conShow transNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformPreview =
  [ testCase "Dry run is the same as normal preview" $
    passIO (conPreview transNoFile) (T.pack "Transformation of: \"test\"")
  , testCase "Child is successfully dry ran" $
    passMockFiles files (conPreview transFile) $
    T.pack
      "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
  , testCase "Child is unsuccessfully dry ran" $
    failMockFiles
      filesMissing
      (conPreview transFile)
      (DocFileError $ NotFound "foo")
  ]

testTransformEvaluate =
  [ testCase "Child is successfully evaluated" $
    passIO (conEvaluate transNoFile) (T.pack "test baz")
  , testCase "Child is unsuccessfully evaluated" $
    failMockFiles
      filesMissing
      (conEvaluate transFile)
      (DocFileError $ NotFound "foo")
  ]

testTransformError =
  [ testGroup "File deps" testTransformErrorFiles
  , testGroup "Preview" testTransformErrorPreview
  , testCase "Show" testTransformErrorShow
  , testGroup "Resolving" testTransformErrorEvaluate
  ]

transErrNoFile =
  transformError (text $ T.pack "test") $ \text ->
    return $ text <> (T.pack " baz")

transErrFile =
  transformError (Snippet "foo") $ \text -> return $ text <> (T.pack " baz")

transErrNoFileFail =
  transformError (text $ T.pack "test") $ \text -> Left $ T.pack "idk"

transErrFileFail = transformError (Snippet "foo") $ \text -> Left $ T.pack "idk"

testTransformErrorFiles =
  [ testCase "Child doesn't depend on anything" $
    passIO (conNeededFiles transErrNoFile) HS.empty
  , testCase "Child depends on a file" $
    passIO (conNeededFiles transErrFile) $ HS.fromList ["foo"]
  , testCase "Child fails and doesn't depend on anything" $
    passIO (conNeededFiles transErrNoFileFail) HS.empty
  , testCase "Child fails and depends on a file" $
    passIO (conNeededFiles transErrFileFail) $ HS.fromList ["foo"]
  ]

testTransformErrorShow =
  conShow transErrNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformErrorPreview =
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

testTransformErrorEvaluate =
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
