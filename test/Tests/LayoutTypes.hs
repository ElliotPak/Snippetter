module Tests.LayoutTypes (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.LayoutBase
import Snippetter.LayoutTypes
import Control.Monad.Trans.Except
import qualified Data.Text as T

ret :: DocResult IO a -> IO (Either DocError a)
ret val = runExceptT val

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
    files <- ret (getNeededFiles $ T.pack "test")
    assertEqual "Should be Right []" (Right []) files

testTextPreview =
    preview 0 (T.pack "test") @?= (T.pack "\"test\"")

testTextPreviewDryRun = do
    preview <- ret (previewDryRun 0 $ T.pack "test")
    assertEqual "Should be Right \"test\"" (Right (T.pack "\"test\"")) preview

testTextResolve = do
    content <- ret (resolve $ T.pack "test")
    assertEqual "Should be Right \"test\"" (Right (T.pack "test")) content
