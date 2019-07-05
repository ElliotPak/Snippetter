{-# LANGUAGE OverloadedStrings #-}

module Tests.Content (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.LayoutBase
import Snippetter.LayoutTypes
import Control.Monad.Trans.Except
import qualified Data.Text as T

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

testSubMacro = []
