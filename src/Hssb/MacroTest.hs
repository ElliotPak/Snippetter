module Hssb.MacroTest where

import Hssb.Layout
import Hssb.Utilities
import Data.List.Split
import System.FilePath

lookupTitle = lookupString "title"
lookupName = lookupString "name"
lookupDesc = lookupString "desc"
lookupLink = lookupString "link"

pageStandard :: MacroParams -> MacroResult
pageStandard params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    return [Snippet "build-snippets/header.html",
            Snippet "resources/snippets/pageStandard.html",
            Replacement "%CONTENT%"
                [Snippet "resources/snippets/title.html",
                 Snippet "resources/snippets/paragraph.html",
                 replaceWithText "%TITLE%" title,
                 replaceWithText "%DESC%" desc,
                 PlainText "%ENTRIES%"
                ]
           ]

entryTitle :: MacroParams -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    return [Snippet "resources/snippets/entryTitle.html",
            replaceWithText "%TITLE%" title,
            replaceWithText "%DESC%" desc,
            replaceWithText "%LINK%" link
           ]

pageTitle :: MacroParams -> MacroResult
pageTitle params = do
    title <- lookupName params
    desc <- lookupDesc params
    entriesFile <- lookupString "entries-file" params
    return [Snippet "resources/snippets/pageTitle.html",
            replaceWithText "%TITLE%" title,
            replaceWithText "%DESC%" desc,
            Replacement "%ENTRIES%"
                [ApplyMacroToFile entryTitle entriesFile]
           ]
