module Hssb.MacroTest where

import Hssb.Layout
import Hssb.Layout.Types
import Hssb.Utilities

lookupTitle = lookupString "title"
lookupName = lookupString "name"
lookupDesc = lookupString "desc"
lookupLink = lookupString "link"

pageStandard :: MacroParams -> MacroResult
pageStandard params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    return [add $ Snippet "build-snippets/header.html",
            add $ Snippet "resources/snippets/pageStandard.html",
            replace "%CONTENT%"
                [add $ Snippet "resources/snippets/title.html",
                 add $ Snippet "resources/snippets/paragraph.html",
                 -- replaceWithText "%TITLE%" title,
                 -- replaceWithText "%DESC%" desc,
                 add $ "%ENTRIES%"
                ]
           ]

entryTitle :: MacroParams -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    return [add $ Snippet "resources/snippets/entryTitle.html",
            replace "%TITLE%" title,
            replace "%DESC%" desc,
            replace "%LINK%" link
           ]

-- pageTitle :: MacroParams -> MacroResult
-- pageTitle params = do
--     title <- lookupName params
--     desc <- lookupDesc params
--     entriesFile <- lookupString "entries-file" params
--     return [Snippet "resources/snippets/pageTitle.html",
--             replaceWithText "%TITLE%" title,
--             replaceWithText "%DESC%" desc,
--             Replacement "%ENTRIES%"
--                 [ApplyMacroToFile entryTitle entriesFile]
--            ]
