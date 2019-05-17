module Hssb.MacroTest where

import Control.Monad.Trans.Except
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Hssb.Layout
import Hssb.Layout.Types
import Hssb.Utilities
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

macroMap :: HashMap String Macro
macroMap = fromList [("title-page", pageTitle)]

displayDocResult :: DocResult IO T.Text -> IO ()
displayDocResult doc = do
    unwrapped <- runExceptT doc
    putStrLn $ show unwrapped

executeBuildAction f mp = loadBuildAction f mp >>= executeSiteAction

lookupTitle = lookupString "title"
lookupName = lookupString "name"
lookupDesc = lookupString "desc"
lookupLink = lookupString "link"

pageStandard :: Params -> MacroResult
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

entryTitle :: Params -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    return [add $ Snippet "resources/snippets/entryTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replaceText "%LINK%" link
           ]

pageTitle :: Params -> MacroResult
pageTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    entries <- lookupString "entries-file" params
    return [add $ Snippet "resources/snippets/pageTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replace "%ENTRIES%" $ add (MacroOnFile entryTitle entries)
           ]
