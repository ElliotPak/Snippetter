{-# LANGUAGE OverloadedStrings #-}

module Tests.LayoutTypes (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.LayoutBase
import Snippetter.LayoutTypes
import Control.Monad.Trans.Except
import qualified Data.Text as T

tests = [ testGroup "Content" testContent
        ]

testContent = 
    [ testGroup "Text" testText
    , testGroup "Snippet" testSnippet
    ]

testText =
    [ testCase "Text file deps" testTextFiles
    , testCase "Text preview" testTextPreview
    , testCase "Text preview dry run" testTextPreviewDryRun
    , testCase "Text resolving" testTextResolve
    ]

testSnippet = 
    [ testCase "Snippet file deps" testSnippetFiles
    , testCase "Snippet preview" testSnippetPreview
    , testGroup "Snippet preview dry run" testSnippetPreviewDryRun
    , testGroup "Snippet resolving" testSnippetResolve
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
        retPassMissing "foo" (previewDryRun 0 $ Snippet "foo") (InvalidPath "foo")
    ]

testSnippetResolve =
    [ testCase "File exists" $
        retPassFileRead "bar" (resolve $ Snippet "foo") (T.pack "bar")
    , testCase "File doesn't exist" $
        retPassMissing "foo" (resolve $ Snippet "foo") (InvalidPath "foo")
    ]

testTextFiles =
    retPassIO (getNeededFiles $ T.pack "test") []

testTextPreview =
    preview 0 (T.pack "test") @?= (T.pack "\"test\"")

testTextPreviewDryRun = do
    retPassIO (previewDryRun 0 $ T.pack "test") (T.pack "\"test\"")

testTextResolve =
    retPassIO (resolve $ T.pack "test") (T.pack "test")
