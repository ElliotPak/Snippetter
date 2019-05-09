module Hssb.MacroTest where

import Control.Monad.Trans.Except
import Hssb.Layout
import Hssb.Layout.Types
import Hssb.Utilities
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

lookupTitle = lookupString "title"
lookupName = lookupString "name"
lookupDesc = lookupString "desc"
lookupLink = lookupString "link"

test :: Macro -> FilePath -> IO ()
test macro path = do
    results <- runExceptT $ executeEntryFile macro path
    case results of
      (Right r) -> TIO.putStr r
      (Left l)  -> putStr $ show l

pageStandard :: MacroParams -> MacroResult
pageStandard params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    return [add $ Snippet "build-snippets/header.html",
            add $ Snippet "resources/snippets/pageStandard.html",
            replace "%CONTENT%"
                [add $ Snippet "resources/snippets/title.html",
                 add $ Snippet "resources/snippets/paragraph.html",
                 replaceText "%TITLE%" title,
                 replaceText "%DESC%" desc,
                 add "%ENTRIES%"
                ]
           ]

entryTitle :: MacroParams -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    return [add $ Snippet "resources/snippets/entryTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replaceText "%LINK%" link
           ]

pageTitle :: MacroParams -> MacroResult
pageTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    entries <- lookupString "entries-file" params
    return [add $ Snippet "resources/snippets/pageTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replace "%ENTRIES%" $ add (MacroOnFile entryTitle entries)
           ]
