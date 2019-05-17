module Hssb.MacroTest where

import Control.Monad.Trans.Except
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Hssb.Layout
import Hssb.Layout.Types
import Hssb.Utilities
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

macroMap :: HashMap String Macro
macroMap = fromList [("title-page", entryTitle)]

displayDocResult :: DocResult IO SiteAction -> IO ()
displayDocResult doc = do
    unwrapped <- runExceptT doc
    putStrLn $ show unwrapped

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
                 replaceText "%TITLE%" title,
                 replaceText "%DESC%" desc,
                 addString "%ENTRIES%"
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

-- pageTitle :: MacroParams -> MacroResult
-- pageTitle params = do
--     title <- lookupTitle params
--     desc <- lookupDesc params
--     entries <- lookupString "entries-file" params
--     return [add $ Snippet "resources/snippets/pageTitle.html",
--             replaceText "%TITLE%" title,
--             replaceText "%DESC%" desc,
--             replace "%ENTRIES%" $ add (MacroOnFile entryTitle entries)
--            ]
