{-# LANGUAGE OverloadedStrings #-}

module Tests.Content (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.LayoutBase
import Snippetter.LayoutTypes
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
    , testCase "Resolving" testTextResolve
    ]

testTextFiles =
    retPassIO (getNeededFiles $ T.pack "test") []

testTextPreview =
    preview 0 (T.pack "test") @?= (T.pack "\"test\"")

testTextPreviewDryRun =
    retPassIO (previewDryRun 0 $ T.pack "test") (T.pack "\"test\"")

testTextResolve =
    retPassIO (resolve $ T.pack "test") (T.pack "test")

testSnippet = 
    [ testCase "File deps" testSnippetFiles
    , testCase "Preview" testSnippetPreview
    , testGroup "Preview Dry Run" testSnippetPreviewDryRun
    , testGroup "Resolving" testSnippetResolve
    ]

testSnippetFiles =
    retPassIO (getNeededFiles $ Snippet "foo") ["foo"]

testSnippetPreview =
    preview 0 (Snippet "foo") @?= (T.pack "Snippet named \"foo\"")

testSnippetPreviewDryRun =
    [ testCase "File exists" $
        retPassFileRead "bar" (previewDryRun 0 $ Snippet "foo") $
            T.pack "Snippet named \"foo\" with the contents:\n    bar"
    , testCase "File doesn't exist" $
        retFailMissing "foo" (previewDryRun 0 $ Snippet "foo") (InvalidPath "foo")
    ]

testSnippetResolve =
    [ testCase "File exists" $
        retPassFileRead "bar" (resolve $ Snippet "foo") (T.pack "bar")
    , testCase "File doesn't exist" $
        retFailMissing "foo" (resolve $ Snippet "foo") (InvalidPath "foo")
    ]

testTransform =
    [ testGroup "File deps" testTransformFiles
    , testCase "Preview" testTransformPreview
    , testGroup "Preview Dry Run" testTransformPreviewDryRun
    , testGroup "Resolving" testTransformResolve
    ]

transNoFile = Transform (T.pack "test") $ \text -> text <> (T.pack " baz")
transFile = Transform (Snippet "foo") $ \text -> text <> (T.pack " baz")

testTransformFiles =
    [ testCase "Child doesn't depend on anything" $
        retPassIO (getNeededFiles transNoFile) []
    , testCase "Child depends on a file" $
        retPassIO (getNeededFiles transFile) ["foo"]
    ]

testTransformPreview =
    preview 0 transNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformPreviewDryRun =
    [ testCase "Dry run is the same as normal preview" $
        retPassIO (previewDryRun 0 transNoFile) (T.pack "Transformation of: \"test\"")
    , testCase "Child is successfully dry ran" $
        retPassFileRead "bar" (previewDryRun 0 transFile) $
            T.pack "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
    , testCase "Child is unsuccessfully dry ran" $
        retFailMissing "foo" (previewDryRun 0 transFile) (InvalidPath "foo")
    ]

testTransformResolve =
    [ testCase "Child is successfully resolved" $
        retPassIO (resolve transNoFile) (T.pack "test baz")
    , testCase "Child is unsuccessfully resolved" $
        retFailMissing "foo" (resolve transFile) (InvalidPath "foo")
    ]

testTransformError =
    [ testGroup "File deps" testTransformErrorFiles
    , testCase "Preview" testTransformErrorPreview
    , testGroup "Preview Dry Run" testTransformErrorPreviewDryRun
    , testGroup "Resolving" testTransformErrorResolve
    ]

transErrNoFile = TransformError (T.pack "test") $ \text -> return $ text <> (T.pack " baz")
transErrFile = TransformError (Snippet "foo") $ \text -> return $ text <> (T.pack " baz")
transErrNoFileFail = TransformError (T.pack "test") $ \text -> Left $ MiscError "idk"
transErrFileFail = TransformError (Snippet "foo") $ \text -> Left $ MiscError "idk"

testTransformErrorFiles =
    [ testCase "Child doesn't depend on anything" $
        retPassIO (getNeededFiles transErrNoFile) []
    , testCase "Child depends on a file" $
        retPassIO (getNeededFiles transErrFile) ["foo"]
    , testCase "Child fails and doesn't depend on anything" $
        retPassIO (getNeededFiles transErrNoFileFail) []
    , testCase "Child fails and depends on a file" $
        retPassIO (getNeededFiles transErrFileFail) ["foo"]
    ]

testTransformErrorPreview =
    preview 0 transErrNoFile @?= (T.pack "Transformation of: \"test\"")

testTransformErrorPreviewDryRun =
    [ testCase "Dry run is the same as normal preview" $
        retPassIO (previewDryRun 0 transErrNoFile) (T.pack "Transformation of: \"test\"")
    , testCase "Child is successfully dry ran" $
        retPassFileRead "bar" (previewDryRun 0 transErrFile) $
            T.pack "Transformation of: \n    Snippet named \"foo\" with the contents:\n        bar"
    , testCase "Child is unsuccessfully dry ran" $
        retFailMissing "foo" (previewDryRun 0 transErrFile) (InvalidPath "foo")
    ]

testTransformErrorResolve =
    [ testCase "Child successfully resolved, transform succeeds" $
        retPassIO (resolve transErrNoFile) (T.pack "test baz")
    , testCase "Child successfully resolved, transform fails" $
        retFailIO (resolve transErrNoFileFail) (MiscError "idk")
    , testCase "Child unsuccessfully resolved, transform succeeds" $
        retFailMissing "foo" (resolve transErrFile) (InvalidPath "foo")
    , testCase "Child unsuccessfully resolved, transform fails" $
        retFailMissing "foo" (resolve transErrFileFail) (InvalidPath "foo")
    ]

testSubMacro =
    [ testGroup "File deps" testSubMacroFiles
    , testGroup "Preview" testSubMacroPreview
    , testGroup "Preview Dry Run" testSubMacroPreviewDryRun
    , testGroup "Resolving" testSubMacroResolve
    ]

basicMacro :: Macro
basicMacro params = do
    temp <- lookupText "title" params
    return [add temp]

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

smEmpty = SubMacro basicMacro H.empty [] []
smEmptySameFile = SubMacro sameFileMacro H.empty [] []
smEmptyFile = SubMacro fileMacro H.empty [] []
smSingle = SubMacro basicMacro H.empty [makePathed params2] []
smSingleFile = SubMacro fileMacro H.empty [makePathed params2] []
smSingleSameFile = SubMacro sameFileMacro H.empty [makePathed params2] []
smMultiple = SubMacro basicMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
smMultipleFile = SubMacro fileMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
smMultipleSameFile = SubMacro sameFileMacro H.empty [makePathed params1, makePathed params2, makePathed params3] []
smDefault = SubMacro basicMacro params1 [makePathed paramsNoTitle] []
smDefault2 = SubMacro basicMacro params2 [makePathed params1] []
smDefaultNoParams = SubMacro basicMacro params1 [] []

testSubMacroFiles =
    [ testCase "Empty, Macro uses no files" $
        retPassIO (getNeededFiles smEmpty) []
    , testCase "Empty, Macro uses the same file constantly" $
        retPassIO (getNeededFiles smEmptySameFile) []
    , testCase "Empty, Macro uses file specified in params" $
        retPassIO (getNeededFiles smEmptyFile) []
    , testCase "Single params, macro uses no files" $
        retPassIO (getNeededFiles smSingle) []
    , testCase "Single params, macro uses same file constantly" $
        retPassIO (getNeededFiles smSingleSameFile) ["foo"]
    , testCase "Single params, macro uses file specified in params" $
        retPassIO (getNeededFiles smSingleFile) ["bar"]
    , testCase "Multiple params, macro uses no files" $
        retPassIO (getNeededFiles smMultiple) []
    , testCase "Multiple params, macro uses same file constantly" $
        retPassIO (getNeededFiles smMultipleSameFile) ["foo"]
    , testCase "Multiple params, macro uses file specified in params" $
        retPassIO (getNeededFiles smMultipleFile) ["foo", "bar", "baz"]
    ]

testSubMacroPreview = []
testSubMacroPreviewDryRun = []
testSubMacroResolve =
    [ testCase "No params" $
        retPassIO (resolve smEmpty) (T.pack "")
    , testCase "No params with default" $
        retPassIO (resolve smDefaultNoParams) (T.pack "")
    , testCase "Single params" $
        retPassIO (resolve smSingle) (T.pack "bar")
    , testCase "Single params with required default" $
        retPassIO (resolve Tests.Content.smDefault) (T.pack "foo")
    , testCase "Single params with overwritten default" $
        retPassIO (resolve smDefault2) (T.pack "foo")
    , testCase "Multiple params" $
        retPassIO (resolve smMultiple) (T.pack "foobarbaz")
    ]
