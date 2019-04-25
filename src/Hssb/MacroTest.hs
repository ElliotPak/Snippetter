module Hssb.MacroTest where

import Hssb.Data
import qualified Data.Text as T

lookupTitle = lookupString "title"
lookupDesc = lookupString "desc"
lookupLink = lookupString "link"

entryTest :: MacroResult
entryTest = do
    snip <- startDoc $ Snippet "title-entry.html" 
    snap <- addContent snip $ Snippet "bigboy"
    return snap

entryMaybeWrapper :: (MacroParams -> MacroResult) -> Maybe MacroParams -> Maybe MacroResult
entryMaybeWrapper func params = 
    case params of
      Just a -> Just $ func a
      Nothing -> Nothing

entryTitle :: MacroParams -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    snip <- startDoc $ Snippet "entry-title.html" 
    docTitled <- addContent snip $ replaceWithText "%TITLE%" title
    docDescd <- addContent snip $ replaceWithText "%DESC%" desc
    docLinked <- addContent snip $ replaceWithText "%LINK%" link
    return docLinked

-- pageSnippet = do
    -- addSnippet "title.html"
    -- addReplacement "CONTENTS" 
    --     addSnippets [ "page-title.html", "page-description.html", "entry-container.html" ]
    --     addReplacement "TITLE" (get map "title")
    --     addReplacement "DESCRIPTION" (get map "desc")
    --     addReplacement "ENTRIES" (macroAcrossFile entryTitle (get map "entries-file"))
