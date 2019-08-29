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

tests =
  [ testGroup "Text" testText
  , testGroup "Snippet" testSnippet
  , testGroup "Transform" testTransform
  , testGroup "TransformError" testTransformError
  , testGroup "SubMacro" Tests.SubBuilder.tests
  ]

testText =
  [ testCase "File deps" testTextFiles
  , testCase "Preview" testTextPreview
  , testCase "Preview" testTextPreview
  , testCase "Resolving" testTextEvaluate
  ]

testTextFiles = retPassIO (conNeededFiles $ text $ T.pack "test") HS.empty

testTextShow = conShow (text $ T.pack "test") @?= (T.pack "\"test\"")

testTextPreview =
  retPassIO (conPreview $ text $ T.pack "test") (T.pack "\"test\"")

testTextEvaluate =
  retPassIO (conEvaluate $ text $ T.pack "test") (T.pack "test")

testSnippet =
  [ testCase "File deps" testSnippetFiles
  , testGroup "Preview" testSnippetPreview
  , testCase "Show" testSnippetShow
  , testGroup "Resolving" testSnippetEvaluate
  ]

testSnippetFiles =
  retPassIO (conNeededFiles $ Snippet "foo") $ HS.fromList ["foo"]

testSnippetShow = conShow (snippet "foo") @?= (T.pack "Snippet named \"foo\"")

testSnippetPreview =
  [ testCase "File exists" $
    retPassFileRead "bar" (conPreview $ Snippet "foo") $
    T.pack "Snippet named \"foo\" with the contents:\n    bar"
  , testCase "File doesn't exist" $
    retFailMissing
      "foo"
      (conPreview $ Snippet "foo")
      (DocFileError $ NotFound "foo")
  ]

testSnippetEvaluate =
  [ testCase "File exists" $
    retPassFileRead "bar" (conEvaluate $ Snippet "foo") (T.pack "bar")
  , testCase "File doesn't exist" $
    retFailMissing
      "foo"
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
    retPassIO (conNeededFiles transNoFile) HS.empty
  , testCase "Child depends on a file" $
    retPassIO (conNeededFiles transFile) $ HS.fromList ["foo"]
  ]

testTransformShow =
  conShow transNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformPreview =
  [ testCase "Dry run is the same as normal preview" $
    retPassIO (conPreview transNoFile) (T.pack "Transformation of: \"test\"")
  , testCase "Child is successfully dry ran" $
    retPassFileRead "bar" (conPreview transFile) $
    T.pack
      "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
  , testCase "Child is unsuccessfully dry ran" $
    retFailMissing "foo" (conPreview transFile) (DocFileError $ NotFound "foo")
  ]

testTransformEvaluate =
  [ testCase "Child is successfully evaluated" $
    retPassIO (conEvaluate transNoFile) (T.pack "test baz")
  , testCase "Child is unsuccessfully evaluated" $
    retFailMissing "foo" (conEvaluate transFile) (DocFileError $ NotFound "foo")
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
    retPassIO (conNeededFiles transErrNoFile) HS.empty
  , testCase "Child depends on a file" $
    retPassIO (conNeededFiles transErrFile) $ HS.fromList ["foo"]
  , testCase "Child fails and doesn't depend on anything" $
    retPassIO (conNeededFiles transErrNoFileFail) HS.empty
  , testCase "Child fails and depends on a file" $
    retPassIO (conNeededFiles transErrFileFail) $ HS.fromList ["foo"]
  ]

testTransformErrorShow =
  conShow transErrNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformErrorPreview =
  [ testCase "Dry run is the same as normal preview" $
    retPassIO (conPreview transErrNoFile) (T.pack "Transformation of: \"test\"")
  , testCase "Child is successfully dry ran" $
    retPassFileRead "bar" (conPreview transErrFile) $
    T.pack
      "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
  , testCase "Child is unsuccessfully dry ran" $
    retFailMissing
      "foo"
      (conPreview transErrFile)
      (DocFileError $ NotFound "foo")
  ]

testTransformErrorEvaluate =
  [ testCase "Child successfully evaluated, transform succeeds" $
    retPassIO (conEvaluate transErrNoFile) (T.pack "test baz")
  , testCase "Child successfully evaluated, transform fails" $
    retFailIO (conEvaluate transErrNoFileFail) (MiscDocError "idk")
  , testCase "Child unsuccessfully evaluated, transform succeeds" $
    retFailMissing
      "foo"
      (conEvaluate transErrFile)
      (DocFileError $ NotFound "foo")
  , testCase "Child unsuccessfully evaluated, transform fails" $
    retFailMissing
      "foo"
      (conEvaluate transErrFileFail)
      (DocFileError $ NotFound "foo")
  ]
