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
    ]

testText = [ testCase "Text file deps" testTextFiles
           , testCase "Text preview" testTextPreview
           , testCase "Text preview dry run" testTextPreviewDryRun
           , testCase "Text resolving" testTextResolve
           ]

testTextFiles = do
    retPass (getNeededFiles $ T.pack "test") []

testTextPreview =
    preview 0 (T.pack "test") @?= (T.pack "\"test\"")

testTextPreviewDryRun = do
    retPass (previewDryRun 0 $ T.pack "test") (T.pack "\"test\"")

testTextResolve = do
    retPass (resolve $ T.pack "test") (T.pack "test")
