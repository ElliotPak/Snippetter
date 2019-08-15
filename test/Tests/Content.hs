{-# LANGUAGE OverloadedStrings #-}

module Tests.Content
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Snippetter.Build
import Snippetter.Helpers
import Snippetter.IO
import Snippetter.Layout
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testGroup "Text" testText
  , testGroup "Snippet" testSnippet
  , testGroup "Transform" testTransform
  , testGroup "TransformError" testTransformError
  , testGroup "SubMacro" testSubMacro
  ]

testText =
  [ testCase "File deps" testTextFiles
  , testCase "Preview" testTextPreview
  , testCase "Preview" testTextPreview
  , testCase "Resolving" testTextEvaluate
  ]

testTextFiles = retPassIO (conNeededFiles $ text $ T.pack "test") []

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

testSnippetFiles = retPassIO (conNeededFiles $ Snippet "foo") ["foo"]

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
    retPassIO (conNeededFiles transNoFile) []
  , testCase "Child depends on a file" $
    retPassIO (conNeededFiles transFile) ["foo"]
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
    retPassIO (conNeededFiles transErrNoFile) []
  , testCase "Child depends on a file" $
    retPassIO (conNeededFiles transErrFile) ["foo"]
  , testCase "Child fails and doesn't depend on anything" $
    retPassIO (conNeededFiles transErrNoFileFail) []
  , testCase "Child fails and depends on a file" $
    retPassIO (conNeededFiles transErrFileFail) ["foo"]
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

testSubMacro =
  [ testGroup "File deps" testSubMacroFiles
  , testGroup "Preview" testSubMacroPreview
  , testGroup "Show" testSubMacroShow
  , testGroup "Resolving" testSubMacroEvaluate
  ]

basicMacro :: Macro
basicMacro params = do
  temp <- lookupText "title" params
  return [add $ text temp]

sameFileMacro :: Macro
sameFileMacro params = return [add $ Snippet "foo"]

fileMacro :: Macro
fileMacro params = do
  temp <- lookupText "title" params
  return [add $ Snippet $ T.unpack temp]

fromText str =
  unObject (Y.decodeEither' (B.pack str) :: Either Y.ParseException Y.Value)
  where
    unObject (Right (Y.Object o)) = o

makePathed o = PathedParams o Nothing

params1 = fromText "title: foo"

params2 = fromText "title: bar"

params3 = fromText "title: baz"

paramsNoTitle = fromText "something: test"

smEmpty = singleSubMacro basicMacro H.empty [] []

smEmptySameFile = singleSubMacro sameFileMacro H.empty [] []

smEmptyFile = singleSubMacro fileMacro H.empty [] []

smSingle = singleSubMacro basicMacro H.empty [makePathed params2] []

smSingleFile = singleSubMacro fileMacro H.empty [makePathed params2] []

smSingleSameFile = singleSubMacro sameFileMacro H.empty [makePathed params2] []

smMultiple =
  singleSubMacro
    basicMacro
    H.empty
    [makePathed params1, makePathed params2, makePathed params3]
    []

smMultipleFile =
  singleSubMacro
    fileMacro
    H.empty
    [makePathed params1, makePathed params2, makePathed params3]
    []

smMultipleSameFile =
  singleSubMacro
    sameFileMacro
    H.empty
    [makePathed params1, makePathed params2, makePathed params3]
    []

smDefault = singleSubMacro basicMacro params1 [makePathed paramsNoTitle] []

smDefault2 = singleSubMacro basicMacro params2 [makePathed params1] []

smDefaultNoParams = singleSubMacro basicMacro params1 [] []

testSubMacroFiles =
  [ testCase "Empty, Macro uses no files" $
    retPassIO (conNeededFiles smEmpty) []
  , testCase "Empty, Macro uses the same file constantly" $
    retPassIO (conNeededFiles smEmptySameFile) []
  , testCase "Empty, Macro uses file specified in params" $
    retPassIO (conNeededFiles smEmptyFile) []
  , testCase "Single params, macro uses no files" $
    retPassIO (conNeededFiles smSingle) []
  , testCase "Single params, macro uses same file constantly" $
    retPassIO (conNeededFiles smSingleSameFile) ["foo"]
  , testCase "Single params, macro uses file specified in params" $
    retPassIO (conNeededFiles smSingleFile) ["bar"]
  , testCase "Multiple params, macro uses no files" $
    retPassIO (conNeededFiles smMultiple) []
  , testCase "Multiple params, macro uses same file constantly" $
    retPassIO (conNeededFiles smMultipleSameFile) ["foo"]
  , testCase "Multiple params, macro uses file specified in params" $
    retPassIO (conNeededFiles smMultipleFile) ["foo", "bar", "baz"]
  ]

testSubMacroShow =
  [ testCase "Empty, Macro uses no files" $ conShow smEmpty @?= ""
  , testCase "Empty, Macro uses the same file constantly" $
    conShow smEmptySameFile @?= ""
  , testCase "Empty, Macro uses file specified in params" $
    conShow smEmptyFile @?= ""
  , testCase "Single params, macro uses no files" $ conShow smSingle @?= ""
  , testCase "Single params, macro uses same file constantly" $
    conShow smSingleSameFile @?= ""
  , testCase "Single params, macro uses file specified in params" $
    conShow smSingleFile @?= ""
  , testCase "Multiple params, macro uses no files" $ conShow smMultiple @?= ""
  , testCase "Multiple params, macro uses same file constantly" $
    conShow smMultipleSameFile @?= ""
  , testCase "Multiple params, macro uses file specified in params" $
    conShow smMultipleFile @?= ""
  ]

testSubMacroPreview =
  [ testCase "Empty, Macro uses no files" $ retPassIO (conPreview smEmpty) ""
  , testCase "Empty, Macro uses the same file constantly" $
    retPassIO (conPreview smEmptySameFile) ""
  , testCase "Empty, Macro uses file specified in params" $
    retPassIO (conPreview smEmptyFile) ""
  , testCase "Single params, macro uses no files" $
    retPassIO (conPreview smSingle) ""
  , testCase "Single params, macro uses same file constantly" $
    retPassIO (conPreview smSingleSameFile) ""
  , testCase "Single params, macro uses file specified in params" $
    retPassIO (conPreview smSingleFile) ""
  , testCase "Multiple params, macro uses no files" $
    retPassIO (conPreview smMultiple) ""
  , testCase "Multiple params, macro uses same file constantly" $
    retPassIO (conPreview smMultipleSameFile) ""
  , testCase "Multiple params, macro uses file specified in params" $
    retPassIO (conPreview smMultipleFile) ""
  ]

testSubMacroEvaluate =
  [ testCase "No params" $ retPassIO (conEvaluate smEmpty) (T.pack "")
  , testCase "No params with default" $
    retPassIO (conEvaluate smDefaultNoParams) (T.pack "")
  , testCase "Single params" $ retPassIO (conEvaluate smSingle) (T.pack "bar")
  , testCase "Single params with required default" $
    retPassIO (conEvaluate Tests.Content.smDefault) (T.pack "foo")
  , testCase "Single params with overwritten default" $
    retPassIO (conEvaluate smDefault2) (T.pack "foo")
  , testCase "Multiple params" $
    retPassIO (conEvaluate smMultiple) (T.pack "foobarbaz")
  ]
