{-# LANGUAGE OverloadedStrings #-}

module Tests.Content (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.IO
import Snippetter.Build
import Snippetter.Layout
import Snippetter.Helpers
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

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
    , testCase "Preview Dry Run" testTextPreviewDryRun
    , testCase "Resolving" testTextEvaluate
    ]

testTextFiles =
    retPassIO (conNeededFiles $ text $ T.pack "test") []

testTextPreview =
    conPreview 0 (text $ T.pack "test") @?= (T.pack "\"test\"")

testTextPreviewDryRun =
    retPassIO (conPreviewDryRun 0 $ text $ T.pack "test") (T.pack "\"test\"")

testTextEvaluate =
    retPassIO (conEvaluate $ text $ T.pack "test") (T.pack "test")

testSnippet = 
    [ testCase "File deps" testSnippetFiles
    , testCase "Preview" testSnippetPreview
    , testGroup "Preview Dry Run" testSnippetPreviewDryRun
    , testGroup "Resolving" testSnippetEvaluate
    ]

testSnippetFiles =
    retPassIO (conNeededFiles $ Snippet "foo") ["foo"]

testSnippetPreview =
    conPreview 0 (snippet "foo") @?= (T.pack "Snippet named \"foo\"")

testSnippetPreviewDryRun =
    [ testCase "File exists" $
        retPassFileRead "bar" (conPreviewDryRun 0 $ Snippet "foo") $
            T.pack "Snippet named \"foo\" with the contents:\n    bar"
    , testCase "File doesn't exist" $
        retFailMissing "foo" (conPreviewDryRun 0 $ Snippet "foo") (DocFileError $ NotFound "foo")
    ]

testSnippetEvaluate =
    [ testCase "File exists" $
        retPassFileRead "bar" (conEvaluate $ Snippet "foo") (T.pack "bar")
    , testCase "File doesn't exist" $
        retFailMissing "foo" (conEvaluate $ Snippet "foo") (DocFileError $ NotFound "foo")
    ]

testTransform =
    [ testGroup "File deps" testTransformFiles
    , testCase "Preview" testTransformPreview
    , testGroup "Preview Dry Run" testTransformPreviewDryRun
    , testGroup "Resolving" testTransformEvaluate
    ]

transNoFile = transform (text $ T.pack "test") $ \text -> text <> (T.pack " baz")
transFile = transform (snippet "foo") $ \text -> text <> (T.pack " baz")

testTransformFiles =
    [ testCase "Child doesn't depend on anything" $
        retPassIO (conNeededFiles transNoFile) []
    , testCase "Child depends on a file" $
        retPassIO (conNeededFiles transFile) ["foo"]
    ]

testTransformPreview =
    conPreview 0 transNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformPreviewDryRun =
    [ testCase "Dry run is the same as normal preview" $
        retPassIO (conPreviewDryRun 0 transNoFile) (T.pack "Transformation of: \"test\"")
    , testCase "Child is successfully dry ran" $
        retPassFileRead "bar" (conPreviewDryRun 0 transFile) $
            T.pack "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
    , testCase "Child is unsuccessfully dry ran" $
        retFailMissing "foo" (conPreviewDryRun 0 transFile) (DocFileError $ NotFound "foo")
    ]

testTransformEvaluate =
    [ testCase "Child is successfully evaluated" $
        retPassIO (conEvaluate transNoFile) (T.pack "test baz")
    , testCase "Child is unsuccessfully evaluated" $
        retFailMissing "foo" (conEvaluate transFile) (DocFileError $ NotFound "foo")
    ]

testTransformError =
    [ testGroup "File deps" testTransformErrorFiles
    , testCase "Preview" testTransformErrorPreview
    , testGroup "Preview Dry Run" testTransformErrorPreviewDryRun
    , testGroup "Resolving" testTransformErrorEvaluate
    ]

transErrNoFile = transformError (text $ T.pack "test") $ \text -> return $ text <> (T.pack " baz")
transErrFile = transformError (Snippet "foo") $ \text -> return $ text <> (T.pack " baz")
transErrNoFileFail = transformError (text $ T.pack "test") $ \text -> Left $ T.pack "idk"
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

testTransformErrorPreview =
    conPreview 0 transErrNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformErrorPreviewDryRun =
    [ testCase "Dry run is the same as normal preview" $
        retPassIO (conPreviewDryRun 0 transErrNoFile) (T.pack "Transformation of: \"test\"")
    , testCase "Child is successfully dry ran" $
        retPassFileRead "bar" (conPreviewDryRun 0 transErrFile) $
            T.pack "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
    , testCase "Child is unsuccessfully dry ran" $
        retFailMissing "foo" (conPreviewDryRun 0 transErrFile) (DocFileError $ NotFound "foo")
    ]

testTransformErrorEvaluate =
    [ testCase "Child successfully evaluated, transform succeeds" $
        retPassIO (conEvaluate transErrNoFile) (T.pack "test baz")
    , testCase "Child successfully evaluated, transform fails" $
        retFailIO (conEvaluate transErrNoFileFail) (MiscDocError "idk")
    , testCase "Child unsuccessfully evaluated, transform succeeds" $
        retFailMissing "foo" (conEvaluate transErrFile) (DocFileError $ NotFound "foo")
    , testCase "Child unsuccessfully evaluated, transform fails" $
        retFailMissing "foo" (conEvaluate transErrFileFail) (DocFileError $ NotFound "foo")
    ]

testSubMacro =
    [ testGroup "File deps" testSubMacroFiles
    , testGroup "Preview" testSubMacroPreview
    , testGroup "Preview Dry Run" testSubMacroPreviewDryRun
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

fromText str = unObject (Y.decodeEither' (B.pack str) :: Either Y.ParseException Y.Value)
    where unObject (Right (Y.Object o)) = o

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
smMultiple = singleSubMacro basicMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
smMultipleFile = singleSubMacro fileMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
smMultipleSameFile = singleSubMacro sameFileMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
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

testSubMacroPreview = []
testSubMacroPreviewDryRun = []
testSubMacroEvaluate =
    [ testCase "No params" $
        retPassIO (conEvaluate smEmpty) (T.pack "")
    , testCase "No params with default" $
        retPassIO (conEvaluate smDefaultNoParams) (T.pack "")
    , testCase "Single params" $
        retPassIO (conEvaluate smSingle) (T.pack "bar")
    , testCase "Single params with required default" $
        retPassIO (conEvaluate Tests.Content.smDefault) (T.pack "foo")
    , testCase "Single params with overwritten default" $
        retPassIO (conEvaluate smDefault2) (T.pack "foo")
    , testCase "Multiple params" $
        retPassIO (conEvaluate smMultiple) (T.pack "foobarbaz")
    ]
